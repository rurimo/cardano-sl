module Main where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Control.DeepSeq (force)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Time.Units (Microsecond, convertUnit)
import           GHC.Exts as IL
import qualified Options.Applicative as Opts
import           System.Random (newStdGen)
import           System.Wlog (LoggerName (..))

import           Mockable.CurrentTime (realTime)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Block.Logic.VAR (verifyAndApplyBlocks)
import           Pos.Core (Block)
import           Pos.Core.Common (BlockCount (..), unsafeCoinPortionFromDouble)
import           Pos.Core.Configuration (genesisBlockVersionData, genesisData, genesisSecretKeys)
import           Pos.Core.Genesis (FakeAvvmOptions (..), GenesisData (..), GenesisInitializer (..), TestnetBalanceOptions (..))
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.Generator.Block (BlockGenParams (..), TxGenParams (..), genBlocksNoApply)
import           Pos.Launcher.Configuration (ConfigurationOptions (..), HasConfigurations, defaultConfigurationOptions, withConfigurationsM)
import           Pos.Txp.Logic.Global (txpGlobalSettings)
import           Pos.Util.Chrono (OldestFirst (..), NE)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo, retrieveCompileTimeInfo)
import           Test.Pos.Block.Logic.Mode (BlockTestMode, TestParams (..), runBlockTestMode)

genesisInitializer :: GenesisInitializer
genesisInitializer = GenesisInitializer
    { giTestBalance = balance
    , giFakeAvvmBalance = FakeAvvmOptions
          { faoCount = 1
          , faoOneBalance = maxBound
          }
    , giAvvmBalanceFactor = unsafeCoinPortionFromDouble 0
    , giUseHeavyDlg = False
    , giSeed = 0
    }

balance :: TestnetBalanceOptions
balance = TestnetBalanceOptions
    { tboPoors = 1
    , tboRichmen = 1
    , tboTotalBalance = maxBound
    , tboRichmenShare = 1
    , tboUseHDAddresses = False
    }

generateBlocks :: HasConfigurations => BlockCount -> BlockTestMode (OldestFirst NE Block)
generateBlocks bCount = do
    g <- liftIO $ newStdGen
    let secretKeys =
            case genesisSecretKeys of
                Nothing ->
                    error "generateBlocks: no genesisSecretKeys"
                Just ks -> ks
    bs <- flip evalRandT g $ genBlocksNoApply
            (BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple secretKeys
                , _bgpBlockCount = bCount
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpTxpGlobalSettings = txpGlobalSettings
                })
            maybeToList
    return $ OldestFirst $ NE.fromList bs


data BenchArgs = BenchArgs
    { baConfigPath :: FilePath
    , baConfigKey  :: Text
    , baBlockCount :: BlockCount
    , baRuns       :: Int
    }

configPathP :: Opts.Parser FilePath
configPathP = Opts.strOption $
       Opts.long "config"
    <> Opts.value "lib/configuration.yaml"
    <> Opts.showDefault
    <> Opts.help "path to yaml configuration file"

configKeyP :: Opts.Parser String
configKeyP = Opts.strOption $
       Opts.long "config-key"
    <> Opts.value "bench-validation"
    <> Opts.showDefault
    <> Opts.help "configuration key"

blockCountP :: Opts.Parser BlockCount
blockCountP = Opts.option (BlockCount <$> Opts.auto) $
       Opts.long "block-count"
    <> Opts.value 2000
    <> Opts.showDefault
    <> Opts.help "number of blocks to generate"

runsP :: Opts.Parser Int
runsP = Opts.option Opts.auto $
       Opts.long "runs"
    <> Opts.short 'r'
    <> Opts.value 10
    <> Opts.showDefault
    <> Opts.help "number of runs"

benchArgsParser :: Opts.Parser BenchArgs
benchArgsParser = BenchArgs
    <$> configPathP
    <*> (T.pack <$> configKeyP)
    <*> blockCountP
    <*> runsP

main :: IO ()
main = do
    args <- Opts.execParser
        $ Opts.info
            (benchArgsParser <**> Opts.helper)
            (Opts.fullDesc <> Opts.progDesc
                (  "The program generates given ammount of blocks and applies them. "
                ++ "Generated blocks will be devided into number buckets specified by `--runs` option. "
                ++ "If you specify `--block-count 1000 --runs 10` it will devide the 1000 blocks into 10 lists each of 100 blocks and run `verifyAndApplyBlocks` for each of them."
                )
            )
    startTime <- realTime
    let co = defaultConfigurationOptions
            { cfoFilePath = baConfigPath args
            , cfoKey = baConfigKey args
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo $(retrieveCompileTimeInfo) $
        withConfigurationsM (LoggerName "verifyAndApplyBlocksBench") co $ \_ ->
            let tp = TestParams
                    { _tpStartTime = Timestamp (convertUnit startTime)
                    , _tpBlockVersionData = genesisBlockVersionData
                    , _tpGenesisInitializer = genesisInitializer
                    }
            in runBlockTestMode tp $ do
                liftIO $ putStrLn ("initialize database" :: String)
                initNodeDBs
                liftIO $ putStrLn ("generating blocks" :: String)
                bs <- generateBlocks (baBlockCount args)
                let bss = divide (baRuns args) bs
                putStrLn ("verifying blocks" :: String)
                times <- map realToFrac <$> traverse validate bss

                putStrLn ("runs: " ++ show (baRuns args) ++ ":" ++ show (length bss) ++ " blockCount: " ++ show (length bs) :: String)
                putStrLn ("distribution: " ++ show (map length bss)) 

                let itimes = drop 3 times
                    mean :: Float
                    mean = avarage itimes
                    stddev = sqrt . (\x -> x / realToFrac (length itimes - 1)) . avarage . map ((**2) . (-) mean) $ itimes
                putStrLn ("verification and application mean time: " ++ show mean ++ "msc stddev: " ++ show stddev :: String)
                traverse_ (putStrLn . shw) times
    where
        shw :: Show a => a -> String
        shw = show

        avarage :: [Float] -> Float
        avarage = (/) <$> sum <*> realToFrac . length

        validate
            :: ( HasConfigurations
               , HasCompileInfo
               )
            => OldestFirst NE Block
            -> BlockTestMode Microsecond 
        validate !blocks = do
            initNodeDBs
            verStart <- realTime
            verifyAndApplyBlocks False blocks >>= \case
                Left exc -> liftIO $ putStrLn $ (show exc :: String)
                Right _  -> return ()
            verEnd <- realTime
            return $ verEnd - verStart

        divide :: NFData a => Int -> OldestFirst NE a -> [OldestFirst NE a]
        divide i as = force $ go (length as `div` i) (IL.toList as)
            where
            go :: Int -> [a] -> [OldestFirst NE a]
            go 0 !bs = [IL.fromList bs]
            go n !bs =
                case splitAt n bs of
                    ([], [])  -> []
                    ([], bs') -> go n bs'
                    (b , [])  -> [IL.fromList b]
                    (b , bs') -> IL.fromList b : go n bs'
