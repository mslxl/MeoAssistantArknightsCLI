{-# LANGUAGE ForeignFunctionInterface #-}

module Maa
  ( MaaTask,
    maaFight,
    maaAward,
    maaVisit,
    maaMall,
    maaInfrast,
    maaRecruit,
    maaVersion,
    maaStart,
  )
where

import Control.Applicative (Alternative)
import Foreign
import Foreign.C.String
import Foreign.C.Types

-- 摸不清该怎么抽象，随便写了

newtype MaaTask = MaaTask
  { execTask :: IO Bool
  }

instance Semigroup MaaTask where
  (MaaTask t1) <> (MaaTask t2) = MaaTask ((&&) <$> t1 <*> t2)

instance Monoid MaaTask where
  mempty = MaaTask $ pure True

-- 刷理智
-- 关卡名 -> 吃多少理智 -> 吃多少原石 -> 刷多少次
maaFight :: String -> Int -> Int -> Int -> MaaTask
maaFight = undefined

-- 每日任务
maaAward :: MaaTask
maaAward = undefined

-- 拜访好友
maaVisit :: MaaTask
maaVisit = undefined

-- 信用商店
maaMall :: MaaTask
maaMall = undefined

-- 基建
maaInfrast :: MaaTask
maaInfrast = undefined

-- 公招
-- 自动公招次数 -> 要点击的 tag -> 要确认开始的 tag -> 是否刷新 3-level tag -> 是否使用加急
maaRecruit :: Int -> [Int] -> [Int] -> Bool -> Bool -> MaaTask
maaRecruit = undefined

maaPenguin :: Int -> MaaTask
maaPenguin = undefined

maaStart :: MaaTask
maaStart = undefined

foreign import ccall "AsstCaller.h AsstGetVersion" version :: IO CString

-- MAA 版本
maaVersion :: IO String
maaVersion = version >>= peekCString
