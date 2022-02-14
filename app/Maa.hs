{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}

module Maa
  ( runTask,
    MaaTask,
    MaaAsstPtr,
    ShiftOrder,
    DroneUse,
    create,
    fight,
    award,
    visit,
    mall,
    infrast,
    recruit,
    version,
    start,
    wakeup,
    connect,
    RecruitConfig,
    maaRecruitMaxTimes,
    maaRecruitSelectLevel,
    maaRecruitConfirmLevel,
    maaRecruitNeedRefresh,
    maaRecruitUseExpedited,
    FightConfig,
    maaFightStage,
    maaFightMaxMecidine,
    maaFightMaxStone,
    maaFightMaxTimes,
    InfrastConfig,
    maaInfrastWorkMode,
    maaInfrastShiftOrder,
    maaInfrastDroneUse,
    maaInfrastMoodThreshold,
  )
where

import Control.Applicative
import Control.Monad
import Data.Default
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Exts (FunPtr (FunPtr))
import System.Directory

type MaaAsstPtr = IntPtr

-- 摸不清该怎么抽象，随便写了

newtype MaaTask = MaaTask
  { runTask :: MaaAsstPtr -> IO (MaaAsstPtr, Bool)
  }

instance Semigroup MaaTask where
  (MaaTask t1) <> (MaaTask t2) = MaaTask $ \ptr -> do
    (asst', a) <- t1 ptr
    (asst'', b) <-
      if a
        then t2 asst'
        else pure (asst', False)
    pure (asst'', a && b)

instance Monoid MaaTask where
  mempty = MaaTask $ \ptr -> pure (ptr, True)

wakeup :: MaaTask
wakeup = MaaTask $ \ptr -> asstAppendStartUp ptr >>= (\cbool -> pure (ptr, toBool cbool))

data FightConfig = FightConfig
  { maaFightStage :: String,
    maaFightMaxMecidine :: Int32,
    maaFightMaxStone :: Int32,
    maaFightMaxTimes :: Int32
  }

instance Default FightConfig where
  def = FightConfig "" 0 0 maxBound

fight :: FightConfig -> MaaTask
fight cfg = MaaTask $ \ptr ->
  let stage = newCString $ maaFightStage cfg
      maxMecidine = CInt $maaFightMaxMecidine cfg
      maxStone = CInt $ maaFightMaxStone cfg
      maxTimes = CInt $ maaFightMaxTimes cfg
   in do
        stage <- stage
        result <- asstAppendFight ptr stage maxMecidine maxStone maxTimes
        return (ptr, toBool result)

-- 每日任务
award :: MaaTask
award = MaaTask $ \ptr -> (ptr,) . toBool <$> asstAppendAward ptr

-- 拜访好友
visit :: MaaTask
visit = MaaTask $ \ptr -> (ptr,) . toBool <$> asstAppendVisit ptr

-- 信用商店
mall :: Bool -> MaaTask
mall withShopping = MaaTask $ \ptr -> (ptr,) . toBool <$> asstAppendMall ptr withShopping

data ShiftOrder = Mfg | Trade | Control | Power | Reception | Office | Dorm
  deriving (Show, Eq)

data DroneUse = NotUse | Money | SyntheticJade | CombatRecord | PureGold | OriginStone | Chip
  deriving (Show, Eq)

-- 基建
-- 工作模式（仅支持1) -> 换班顺序
data InfrastConfig = InfrastConfig
  { maaInfrastWorkMode :: Int32,
    maaInfrastShiftOrder :: [ShiftOrder],
    maaInfrastDroneUse :: DroneUse,
    maaInfrastMoodThreshold :: Double
  }

instance Default InfrastConfig where
  def = InfrastConfig 1 [Mfg, Trade, Control, Power, Reception, Office, Dorm] Money 0.3

infrast :: InfrastConfig -> MaaTask
infrast cfg = MaaTask $ \ptr ->
  let workMode = CInt $ maaInfrastWorkMode cfg
      shiftOrder = mapM (newCString . show) (maaInfrastShiftOrder cfg) >>= newArray
      shiftOrderLen = CInt . fromIntegral $ length $ maaInfrastShiftOrder cfg
      droneUse = newCString $ (\str -> if str == "NotUse" then "_NotUse" else str) $ show $ maaInfrastDroneUse cfg
      threshold = maaInfrastMoodThreshold cfg
   in do
        when (workMode /= 1) $ error "Work Mode only support 1 currently"
        shiftOrder <- shiftOrder
        droneUse <- droneUse
        result <- asstAppendInfrast ptr workMode shiftOrder shiftOrderLen droneUse threshold
        return (ptr, toBool result)

data RecruitConfig = RecruitConfig
  { maaRecruitMaxTimes :: Int32,
    maaRecruitSelectLevel :: [Int32],
    maaRecruitConfirmLevel :: [Int32],
    maaRecruitNeedRefresh :: Bool,
    maaRecruitUseExpedited :: Bool
  }

instance Default RecruitConfig where
  def = RecruitConfig 3 [4] [1, 2, 3, 4] True False

-- 公招
-- 自动公招次数 -> 要点击的 tag -> 要确认开始的 tag -> 是否刷新 3-level tag -> 是否使用加急
recruit :: RecruitConfig -> MaaTask
recruit cfg = MaaTask $ \ptr ->
  let selectLevel = newArray $ map CInt $ maaRecruitSelectLevel cfg
      confirmLevel = newArray $ map CInt $ maaRecruitConfirmLevel cfg
      selectLevenLen = CInt . fromIntegral $length $ maaRecruitSelectLevel cfg
      confirmLevelLen = CInt . fromIntegral $ length $ maaRecruitConfirmLevel cfg
      needRefresh = maaRecruitNeedRefresh cfg
      useExpedited = maaRecruitUseExpedited cfg
      maxTimes = CInt $ maaRecruitMaxTimes cfg
   in do
        selectLevel <- selectLevel
        confirmLevel <- confirmLevel
        result <- asstAppendRecruit ptr maxTimes selectLevel selectLevenLen confirmLevel confirmLevelLen needRefresh useExpedited
        return (ptr, toBool result)

penguin :: String -> MaaTask
penguin pid = MaaTask $ \ptr -> (ptr,) . toBool <$> (newCString pid >>= asstSetPenguinId ptr)

start :: MaaTask
start = MaaTask $ \ptr -> asstStart ptr >>= (\cbool -> pure (ptr, toBool cbool))

connect :: String -> MaaTask
connect address = MaaTask $ \ptr -> do
  let a = toBool <$> (newCString address >>= asstCatchCustom ptr)
  (ptr,) <$> a

-- MAA 版本
version :: IO String
version = asstGetVersion >>= peekCString

create :: IO MaaAsstPtr
create =
  let asstDir = maaAsstDir >>= newCString
   in do
        asstDir <- asstDir
        asstCreate asstDir

maaAsstDir :: IO FilePath
maaAsstDir = (++ "/lib") <$> getCurrentDirectory

foreign import ccall "AsstCaller.h AsstGetVersion" asstGetVersion :: IO CString

foreign import ccall "AsstCaller.h AsstCreate" asstCreate :: CString -> IO MaaAsstPtr

foreign import ccall "AsstCaller.h AsstAppendStartUp" asstAppendStartUp :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstStart" asstStart :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendAward" asstAppendAward :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstCatchCustom" asstCatchCustom :: MaaAsstPtr -> CString -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendFight" asstAppendFight :: MaaAsstPtr -> CString -> CInt -> CInt -> CInt -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendMall" asstAppendMall :: MaaAsstPtr -> Bool -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendVisit" asstAppendVisit :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstSetPenguinId" asstSetPenguinId :: MaaAsstPtr -> CString -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendInfrast" asstAppendInfrast :: MaaAsstPtr -> CInt -> Ptr CString -> CInt -> CString -> Double -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendRecruit" asstAppendRecruit :: MaaAsstPtr -> CInt -> Ptr CInt -> CInt -> Ptr CInt -> CInt -> Bool -> Bool -> IO CBool
