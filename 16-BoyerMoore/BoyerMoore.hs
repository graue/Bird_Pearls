import Data.List (inits, isPrefixOf)

--   matches "abcab" "ababcabcab" => [7,10]

-- Naive implementation of matches.
naiveMatches :: Eq a => [a] -> [a] -> [Int]
naiveMatches ws = map length . filter (endswith ws) . inits
                  where
                    endswith ws xs = reverse ws `isPrefixOf` reverse xs

-- Basic Boyer-Moore implementation, but still takes Omega(mn) steps in
-- worst case.
basicMatches :: Eq a => [a] -> [a] -> [Int]
basicMatches ws = map fst . filter ((sw `isPrefixOf`) . snd) .
                    scanl step (0, [])
                  where
                    sw = reverse ws
                    step (n, sx) x = (n+1, x:sx)

-- Several insights applied to the above:
--
-- Scan lemma:
--   map (foldl op e) . inits = scanl op e
--
-- Fork law (where map composed with filter):
--   map f . filter p = map fst . filter snd . map (fork (f, p))
--                      where fork (f, p) x = (f x, p x)]
-- Generalized:
--   map f . filter (p . g) = map fst . filter (p . snd) . map (fork (f, g))
--
-- Tupling law of foldl:
--   fork (foldl op1 e1, foldl op2 e2) = foldl op (e1, e2)
--     where op (a, b) x = (op1 a x, op2 b x)
--
-- Reverse restated as a fold:
--   reverse = foldl (flip (:)) []
--
-- In the above code, "step" is "fork", with op1 being length, and op2 being
-- reverse.
--
-- On "shifting", page 120
