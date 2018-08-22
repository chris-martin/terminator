import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Terminator/Terminated.hs"
    , "src/Terminator/TerminatedMaybe.hs"
    ]
