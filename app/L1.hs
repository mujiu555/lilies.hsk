module L1 where

data Values
  = ViSymbol       String                     -- ^                       Ordinary Identifier, resolved by context
  | ViInteger      Integer                    -- ^                                         Signed 2's Completion Integer
  | ViReal         Double                     -- ^                                         IEEE 754 Binary Float-Point Real
  | ViBoolean      Bool                       -- ^                                         Boolean
  | ViCharacter    Char                       -- ^                                         Character
  | ViString       String                     -- ^                                         String, escape interpreted
