{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Prelude hiding ( readFile, writeFile )
import Data.Csv
import Data.ByteString.Lazy.Char8 ( ByteString
                                  , readFile
                                  , writeFile )
import Data.Vector ( Vector, (!), toList )

import StateInfo ( getStateInfo )

data Transaction = Transaction { tid :: !String
                               , paymentID :: !String
                               , amount :: !String
                               , fee :: !String
                               , refundedAmount :: !String
                               , userID :: !String
                               , firstName :: !String
                               , lastName :: !String
                               , email :: !String
                               , city :: !String
                               , state :: !String
                               , zipCode :: !String
                               , employer :: !String
                               , referrer :: !String
                               , courseID :: !String
                               , startTime :: !String
                               , endTime :: !String
                               , created :: !String
                               , updated :: !String
                               , utcTime :: !String
                               } deriving (Show, Eq)

instance FromNamedRecord Transaction where
    parseNamedRecord p = Transaction <$>
                         p .: "ID" <*>
                         p .: "Payment ID" <*>
                         p .: "Amount" <*>
                         p .: "Fee" <*>
                         p .: "Refunded Amount" <*>
                         p .: "User ID" <*>
                         p .: "First Name" <*>
                         p .: "Last Name" <*>
                         p .: "e-Mail" <*>
                         p .: "City" <*>
                         p .: "State" <*>
                         p .: "ZIP" <*>
                         p .: "Employer" <*>
                         p .: "Referrer" <*>
                         p .: "Course ID" <*>
                         p .: "Start Time" <*>
                         p .: "End Time" <*>
                         p .: "Created" <*>
                         p .: "Updated" <*>
                         p .: "UTC Time"

instance ToNamedRecord Transaction where
    toNamedRecord t = namedRecord [ "ID" .= tid t
                                  , "Payment ID" .= paymentID t
                                  , "Amount" .= amount t
                                  , "Fee" .= fee t
                                  , "Refunded Amount" .= refundedAmount t
                                  , "User ID" .= userID t
                                  , "First Name" .= firstName t
                                  , "Last Name" .= lastName t
                                  , "e-Mail" .= email t
                                  , "City" .= city t
                                  , "State" .= state t
                                  , "ZIP" .= zipCode t
                                  , "Employer" .= employer t
                                  , "Referrer" .= referrer t
                                  , "Course ID" .= courseID t
                                  , "Start Time" .= startTime t
                                  , "End Time" .= endTime t
                                  , "Created" .= created t
                                  , "Updated" .= updated t
                                  , "UTC Time" .= utcTime t
                                  ]

fixTransaction :: Transaction -> IO Transaction
fixTransaction trans =
    if null (zipCode trans)
    then do print "Not enough info for transaction; returning."
            print trans --Can't modify with insufficient info.
            return trans
    else do print $ "Modifying transaction ID: " ++ tid trans
            stateName <- getStateInfo $ zipCode trans
            return $ trans {state=stateName}

fixCSV :: Either String (Header, Vector Transaction) -> IO ByteString
fixCSV t = case t of
                Left err -> error err
                Right (h, transactions) -> do
                    newTransactions <- mapM fixTransaction transactions
                    return $ encodeByName h (toList newTransactions)

main :: IO ()
main = do
    fixed <- readFile "crowdman.csv" >>= fixCSV . decodeByName
    writeFile "crowdman_fixed.csv" fixed
