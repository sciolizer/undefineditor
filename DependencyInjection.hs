{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DependencyInjection (
  Binding(),
  Injector(),
  Constructor,
  constructor,
  constructed,
  injector,
  instantiate
) where

import Prelude hiding (lookup)

import Control.Applicative hiding (empty)
import Control.Monad.Except
import Control.Monad.State
import Data.Dynamic
import Data.Functor.Identity
import Data.IORef
import Data.List hiding (insert, lookup)
import Data.Map hiding (foldl)

data Binding = Requires [TypeRep] TypeRep ([Dynamic] -> IO Dynamic)

instance Show Binding where
  show (Requires trs tr _) = "Requires [" ++ intercalate "," (fmap show trs) ++ "] " ++ show tr

data BindingError
  = DuplicateBinding TypeRep String {- [Binding] -}
  | UnsatisfiedDependency TypeRep -- todo: [TypeRep] -- list is chain back to the instantiation request, with the last item being the instantiation request, or an empty list if there is no binding for the requested instantiation
  deriving (Eq, Show)

key :: Binding -> TypeRep
key (Requires _ t _) = t

mkMap :: [Binding] -> TypeRep -> [Binding]
mkMap = foldl (\mp b tr -> (if key b == tr then [b] else []) ++ mp tr) (const []) where

create :: (TypeRep -> [Binding]) -> TypeRep -> StateT BindingMap IO (Either BindingError Dynamic)
create allBindings = runExceptT . m where
  m :: TypeRep -> ExceptT BindingError (StateT BindingMap IO) Dynamic
  m tr = do
    mbDyn <- gets (lookup tr)
    case mbDyn of
      Just d -> return d
      Nothing -> do
        case allBindings tr of
          [] -> throwError (UnsatisfiedDependency tr)
          xx@(_:_:_) -> do
            liftIO $ print xx
            throwError (DuplicateBinding tr (show xx))
          [Requires deps _ builder] -> do
            dyns <- mapM m deps
            result <- liftIO $ builder dyns
            modify (insert tr result)
            return result

constructed :: (Typeable a) => a -> Binding
constructed = constructor . Identity

class Constructor c where
  inputs :: c -> [TypeRep]
  output :: c -> TypeRep
  creator :: c -> [Dynamic] -> IO Dynamic
  -- | Requires [TypeRep] TypeRep ([Dynamic] -> IO Dynamic)

instance (Typeable a) => Constructor (Identity a) where
  inputs _ = []
  output = typeRep
  creator (Identity v) _ = return (toDyn v)

instance (Typeable a) => Constructor (IO a) where
  inputs _ = []
  output = typeRep
  creator v _ = toDyn <$> v

instance forall a b. (Typeable b, Constructor a) => Constructor (b -> a) where
  inputs f = (typeRep (Proxy :: Proxy b)) : (inputs (f undefined))
  output f = output (f undefined)
  creator _ [] = error $ "function call with not enough dyn arguments"
  creator f (x:xs) =
    case fromDynamic x of
      Nothing -> error $ "function given wrong type"
      Just y -> creator (f y) xs

constructor :: Constructor c => c -> Binding
constructor c = Requires (inputs c) (output c) (creator c)

{-
factory :: Constructor c => Int -> c -> Binding
factory = undefined
we actually don't really need factory, since you can just do
  \inj1 inj2 inj3 -> Identity (\spec1 spec2 -> ...)
-}

type BindingMap = Map TypeRep Dynamic

data Injector = Injector
  (IORef BindingMap)
  (TypeRep -> StateT BindingMap IO (Either BindingError Dynamic))

injector :: [Binding] -> IO Injector
injector bs = do
  bmap <- newIORef empty
  return $ Injector bmap (create (mkMap bs))

instantiate :: forall a. Typeable a => Injector -> IO (Either BindingError a)
instantiate (Injector bmapRef m) = do
  bmap <- readIORef bmapRef
  (ebd, newMap) <- runStateT (m (typeRep (Proxy :: Proxy a))) bmap
  writeIORef bmapRef newMap
  let unDyn d = case fromDynamic d of { Just x -> x; Nothing -> error "wrong type came out" }
  return . fmap unDyn $ ebd

{-
instantiate :: forall a. Typeable a => [Binding] -> IO (Either BindingError a)
instantiate bindings = do
  let allBindings = mkMap bindings
  ebd <- create allBindings (typeRep (Proxy :: Proxy a))
  let unDyn d = case fromDynamic d of { Just x -> x; Nothing -> error "wrong type came out" }
  return . fmap unDyn $ ebd
  -}

tt :: IO ()
tt = do
  ii [constructed (5 :: Int)] --> Right (5 :: Int)
  (ii [] :: IO (Either BindingError ())) --> Left (UnsatisfiedDependency (typeRep (Proxy :: Proxy ())))
  ii [constructed 'a', constructor (Identity . (:[]) :: Char -> Identity String)] --> Right "a"
  ii [constructed 'a', constructed (5 :: Int), constructor (\c i -> Identity $ [c] ++ show (i :: Int))] --> Right "a5"
  ii
    [ constructor (\c i -> return ([c] ++ show (i :: Int)) :: IO String)
    , constructor (Identity . (fromInteger :: Integer -> Int))
    , constructed 'a'
    , constructed (7 :: Integer) ] --> Right "a7" -- test incorrctly erporting duplicate binding of int =(

ii :: Typeable a => [Binding] -> IO (Either BindingError a)
ii bs = instantiate =<< injector bs

(-->) :: (Show a, Eq a) => IO a -> a -> IO ()
x --> y = do
  xx <- x
  case xx == y of
    True -> return ()
    False -> ioError . userError $ show xx ++ " /= " ++ show y

{-
curious idea:
  what if all application state was kept in (wrapped) TVars?
  Then it would be easy to make lightweight threads that listen
  for changes on those TVars, and serialize them.
-}
