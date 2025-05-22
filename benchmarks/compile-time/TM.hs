{-# LANGUAGE TemplateHaskell #-}

module TH (main) where

import TypeMachine

$(type_ "RecordTM50" (record (("f_" ++) . show <$> [0 .. 49 :: Int]) [t|Int|]))

{-# NOINLINE mkRecordTM50 #-}
mkRecordTM50 :: RecordTM50
mkRecordTM50 =
    RecordTM50
        0
        1
        2
        3
        4
        5
        6
        7
        8
        9
        10
        11
        12
        13
        14
        15
        16
        17
        18
        19
        20
        21
        22
        23
        24
        25
        26
        27
        28
        29
        30
        31
        32
        33
        34
        35
        36
        37
        38
        39
        40
        41
        42
        43
        44
        45
        46
        47
        48
        49

sumRecordTM50 :: RecordTM50 -> Int
{-# NOINLINE sumRecordTM50 #-}
sumRecordTM50
    ( RecordTM50
            f0
            f1
            f2
            f3
            f4
            f5
            f6
            f7
            f8
            f9
            f10
            f11
            f12
            f13
            f14
            f15
            f16
            f17
            f18
            f19
            f20
            f21
            f22
            f23
            f24
            f25
            f26
            f27
            f28
            f29
            f30
            f31
            f32
            f33
            f34
            f35
            f36
            f37
            f38
            f39
            f40
            f41
            f42
            f43
            f44
            f45
            f46
            f47
            f48
            f49
        ) =
        ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (0 + f0)
                                                                                                            + f1
                                                                                                      )
                                                                                                        + f2
                                                                                                    )
                                                                                                        + f3
                                                                                                  )
                                                                                                    + f4
                                                                                                )
                                                                                                    + f5
                                                                                              )
                                                                                                + f6
                                                                                            )
                                                                                                + f7
                                                                                          )
                                                                                            + f8
                                                                                        )
                                                                                            + f9
                                                                                      )
                                                                                        + f10
                                                                                    )
                                                                                        + f11
                                                                                  )
                                                                                    + f12
                                                                                )
                                                                                    + f13
                                                                              )
                                                                                + f14
                                                                            )
                                                                                + f15
                                                                          )
                                                                            + f16
                                                                        )
                                                                            + f17
                                                                      )
                                                                        + f18
                                                                    )
                                                                        + f19
                                                                  )
                                                                    + f20
                                                                )
                                                                    + f21
                                                              )
                                                                + f22
                                                            )
                                                                + f23
                                                          )
                                                            + f24
                                                        )
                                                            + f25
                                                      )
                                                        + f26
                                                    )
                                                        + f27
                                                  )
                                                    + f28
                                                )
                                                    + f29
                                              )
                                                + f30
                                            )
                                                + f31
                                          )
                                            + f32
                                        )
                                            + f33
                                      )
                                        + f34
                                    )
                                        + f35
                                  )
                                    + f36
                                )
                                    + f37
                              )
                                + f38
                            )
                                + f39
                          )
                            + f40
                        )
                            + f41
                      )
                        + f42
                    )
                        + f43
                  )
                    + f44
                )
                    + f45
              )
                + f46
            )
                + f47
          )
            + f48
        )
            + f49

main :: IO ()
main = print (sumRecordTM50 mkRecordTM50)
