# Type Machine

TypeScript offers [*Utility Types*](https://www.typescriptlang.org/docs/handbook/utility-types.html), which allows creating a type from another.
There is no way of doing this in Haskell. You have to maintain all your types yourselves, and handle conversions from one to another yourself.

`type-machine` brings a solution to this problem. Using Template Haskell, generate new types using Type-Script-inspired functions like `omit`, `pick` and `record`.
It can also generate a conversion type-class that allows you to access fields and convert one type to another.

- Requirements
    - Requires a couple of language extensions (see example)
    - Input ADT must have exactly one record constructor
- Limitations
    - Does not support type parameters yet
    - `require` and `partial` only work with `Maybe` fields 

## Examples

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

data User = User {
    id :: Int,
    name :: String,
    email :: Maybe String
}

$(type_ "UserWithEmail" (required ["email"] <::> ''User))
-- data UserWithEmail = UserWithEmail {
--     id :: Int,
--     name :: String,
--     email :: String
-- }

$(type_ "UserWithoutId" (omit ["id"] <::> ''User))
-- data UserWithoutId = UserWithoutId {
--     name :: String,
--     email :: String
-- }

$(type_ "UserId" (pick ["id"] <::> ''User))
-- data UserId = UserId {
--     id :: Int
-- }

$(type_ "Vector3" (record ["x", "y", "z"] [t|Int|]))
-- data Vector3 = Vector3 {
--     x :: Int,
--     y :: Int,
--     z :: Int
-- }

-----
-- Type Parameters
-----

data MyMaybe a = { content :: Maybe a }

$(type_ "MyString" (apply [t|String|] <::> ''MyMaybe))
-- data MyString = MyString { 
--     content :: Maybe String
-- }

-----
-- Is
-----

$(declareIs ''User)
-- class IsUser a where
--     getId :: a -> Int
--     getName :: a -> String
--     getEmail :: a -> String
--     setId :: Int -> a -> a
--     setName :: String -> a -> a
--     setEmail :: String -> a -> a
--
-- instance IsUser User where
--     getId = id
--     getName = name
--     getEmail = email
--     setId = ...
--     setName = ...
--     setEmail = ...

$(type_ "UserWithoutEmail" (omit ["email"] <::> ''User))
$(deriveIs ''User ''UserWithoutEmail)
-- instance IsUser UserWithoutEmail where
--     ...

$(type_ "UserWithoutId" (omit ["id"] <::> ''User))
$(deriveIs ''User ''UserWithoutId) -- Will fail
```
