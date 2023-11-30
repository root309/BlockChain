import Data.Time.Clock.POSIX (getPOSIXTime)
import Crypto.Hash (hash, Digest, SHA256)
import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash as Crypto

-- Block Definition
data Block = Block {
    index :: Int,
    timestamp :: String,
    transactions :: String,
    previousHash :: String,
    blockHash :: String
} deriving (Show)

-- Hash Function
hashBlock :: Block -> String
hashBlock block = show (Crypto.hash (C.pack (show block)) :: Digest SHA256)

-- Generation of Genesis Block
createGenesisBlock :: IO Block
createGenesisBlock = do
    currentTime <- getCurrentTime
    return Block {
        index = 0,
        timestamp = currentTime,
        transactions = "Genesis Block",
        previousHash = "0",
        blockHash = "0"
    }

-- Generate new block
createNewBlock :: Block -> String -> IO Block
createNewBlock previousBlock newTransactions = do
    currentTime <- getCurrenTime
    let newIndex = index previousBlock + 1
    let newBlock = Block {
        index = newIndex,
        timestamp = currentTime,
        transactions = newTransactions,
        previousHash = blockHash previousBlock,
        blockHash = ""
    }
    let newHash = hashBlock newBlock
    return newBlock { blockHash = newHash }

-- Get current time as string
getCurrentTime :: IO String
getCurrentTime = show <$> getPOSIXTime

main :: IO ()
main = do
    genesisBlock <- createGenesisBlock
    putStrLn "Genesis Block:"
    print genesisBlock

    newBlock <- createNewBlock genesisBlock "Some transactions"
    putStrLn "\nNew Block:"
    print newBlock

