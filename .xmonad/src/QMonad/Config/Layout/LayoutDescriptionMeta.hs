module QMonad.Config.Layout.LayoutDescriptionMeta (
  layoutMeta,
  parseLayoutDescription,
) where

import XMonad

-- | Layout meta
-- Useful hack to pass extra information about layouts by putting invisible elements in the description

layoutMeta :: (LayoutClass l a) => Maybe String -> l a -> String
layoutMeta (Just s) l = "&`" ++ s ++ "`" ++ description l
layoutMeta Nothing l = description l

-- | Parse special meta from layout description
-- Returns ([meta], remain)
parseLayoutDescription :: [String] -> String -> ([String], String)
parseLayoutDescription m [] = (m, [])
parseLayoutDescription m s = case e of
      Just e' -> parseLayoutDescription (m ++ [e']) s'
      Nothing -> (m, s')
  where (e,s') = readLayoutMeta s

readLayoutMeta :: String -> (Maybe String, String)
readLayoutMeta ('&':'`':xs) = (Just s, rest)
  where (s,r) = span (/= '`') xs
        rest = drop 1 r
readLayoutMeta s = (Nothing, s)
