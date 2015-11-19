module DependencyInjection where

data Binding a

instance :: Dynamic -> Binding
instance = undefined

class Constructor f

instance Constructor (IO Dynamic)
instance Constructor a => Constructor (b -> a)

constructor :: Constructor c => c -> Binding
constructor = undefined

-- factory takes two args
--   a function signature
--   a function which has that function signature as a suffix
--   returns a binding
-- factory :: b
