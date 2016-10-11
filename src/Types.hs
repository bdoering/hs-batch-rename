module Types
       ( Image(..)
       , ImageGroup(..) )
       where

import Data.Time

data Image = Image { exifDateTime :: Maybe LocalTime
                   , modificationDateTime :: LocalTime
                   , filepath :: FilePath }
             deriving Show

-- Combine images with a common basename into one group
data ImageGroup = ImageGroup { basename :: String -- ^Filename with path, without extension
                             , groupDateTime :: LocalTime -- ^Representative date
                             , images :: [Image] }
                  deriving Show

instance Eq ImageGroup where
  (==) (ImageGroup b1 _ _) (ImageGroup b2 _ _) = b1 == b2

-- First order by EXIF date, and if not available, then by
-- modification date (first in list)
instance Ord ImageGroup where
  compare x y = compare (groupDateTime x) (groupDateTime y)
