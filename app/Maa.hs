{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}

module Maa
  ( runTask,
    MaaTask,
    MaaAsstPtr,
    ShiftOrder,
    DroneUse,
    maaAsstCreate,
    maaFight,
    maaAward,
    maaVisit,
    maaMall,
    maaInfrast,
    maaRecruit,
    maaVersion,
    maaStart,
    maaWakeup,
    maaConnect,
  )
where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Exts (maxTupleSize)
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

maaWakeup :: MaaTask
maaWakeup = MaaTask $ \ptr -> asstAppendStartUp ptr >>= (\cbool -> pure (ptr, toBool cbool))

-- 刷理智
-- 关卡名 -> 吃多少理智 -> 吃多少原石 -> 刷多少次
maaFight :: String -> Int -> Int -> Int -> MaaTask
maaFight stageName san or times = MaaTask $ \ptr -> do
  st <- newCString stageName
  (ptr,) . toBool <$> asstAppendFight ptr st san or times

-- 每日任务
maaAward :: MaaTask
maaAward = MaaTask $ \ptr -> (ptr,) . toBool <$> asstAppendAward ptr

-- 拜访好友
maaVisit :: MaaTask
maaVisit = MaaTask $ \ptr -> (ptr,) . toBool <$> asstAppendVisit ptr

-- 信用商店
maaMall :: Bool -> MaaTask
maaMall withShopping = MaaTask $ \ptr -> (ptr,) . toBool <$> asstAppendMall ptr withShopping

data ShiftOrder = Mfg | Trade | Control | Power | Reception | Office | Dorm
  deriving (Show, Eq)

data DroneUse = NotUse | Money | SyntheticJade | CombatRecord | PureGold | OriginStone | Chip
  deriving (Show, Eq)

-- 基建
-- 工作模式（仅支持1) -> 换班顺序
maaInfrast :: Int -> [ShiftOrder] -> DroneUse -> Double -> MaaTask
maaInfrast workMode order droneUse threshold = MaaTask $ \ptr -> do
  -- 说实话丑死了
  when (workMode /= 1) $ error "Work Mode only support 1 currently"
  shiftOrder <- mapM (newCString . show) order >>= newArray
  droneUseC <- newCString $ (\str -> if str == "NotUse" then "_NotUse" else str) $ show droneUse
  result <- asstAppendInfrast ptr workMode shiftOrder (length order) droneUseC threshold
  return (ptr, toBool result)

-- 公招
-- 自动公招次数 -> 要点击的 tag -> 要确认开始的 tag -> 是否刷新 3-level tag -> 是否使用加急
maaRecruit :: Int -> [Int] -> [Int] -> Bool -> Bool -> MaaTask
maaRecruit maxTimes selectLevel confirmLevel needRefresh useExpedited = MaaTask $ \ptr -> do
  levelC <- newArray selectLevel
  confirmC <- newArray confirmLevel
  result <- asstAppendRecruit ptr maxTimes levelC (length selectLevel) confirmC (length confirmLevel) needRefresh useExpedited
  return (ptr, toBool result)

maaPenguin :: String -> MaaTask
maaPenguin pid = MaaTask $ \ptr -> (ptr,) . toBool <$> (newCString pid >>= asstSetPenguinId ptr)

maaStart :: MaaTask
maaStart = MaaTask $ \ptr -> asstStart ptr >>= (\cbool -> pure (ptr, toBool cbool))

maaConnect :: String -> MaaTask
maaConnect address = MaaTask $ \ptr -> do
  let a = toBool <$> (newCString address >>= asstCatchCustom ptr)
  (ptr,) <$> a

-- MAA 版本
maaVersion :: IO String
maaVersion = asstGetVersion >>= peekCString

maaAsstCreate :: IO MaaAsstPtr
maaAsstCreate = maaAsstDir >>= newCString >>= asstCreate

maaAsstDir :: IO FilePath
maaAsstDir = (++ "/lib") <$> getCurrentDirectory

foreign import ccall "AsstCaller.h AsstGetVersion" asstGetVersion :: IO CString

foreign import ccall "AsstCaller.h AsstCreate" asstCreate :: CString -> IO MaaAsstPtr

foreign import ccall "AsstCaller.h AsstAppendStartUp" asstAppendStartUp :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstStart" asstStart :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendAward" asstAppendAward :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstCatchCustom" asstCatchCustom :: MaaAsstPtr -> CString -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendFight" asstAppendFight :: MaaAsstPtr -> CString -> Int -> Int -> Int -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendMall" asstAppendMall :: MaaAsstPtr -> Bool -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendVisit" asstAppendVisit :: MaaAsstPtr -> IO CBool

foreign import ccall "AsstCaller.h AsstSetPenguinId" asstSetPenguinId :: MaaAsstPtr -> CString -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendInfrast" asstAppendInfrast :: MaaAsstPtr -> Int -> Ptr CString -> Int -> CString -> Double -> IO CBool

foreign import ccall "AsstCaller.h AsstAppendRecruit" asstAppendRecruit :: MaaAsstPtr -> Int -> Ptr Int -> Int -> Ptr Int -> Int -> Bool -> Bool -> IO CBool
