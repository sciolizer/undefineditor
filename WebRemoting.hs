module WebRemoting where

type Port = Int

data WebRemotingListener

type Javascript = String

newWebRemotingListener :: Javascript -> IO WebRemotingListener
newWebRemotingListener = undefined

data JSRemote a

remote :: JSRemote a -> IO a
remote = undefined

eval :: JSRemote (String -> IO ())
eval = undefined

class JSSerializable a

instance JSON a => JSSerializable a


