interface InternalProgram
    exposes [InternalProgram, fromEffect, toEffect]
    imports []

InternalProgram := Effect U8

fromEffect : Effect U8 -> InternalProgram
fromEffect = @InternalProgram

toEffect : InternalProgram -> Effect U8
toEffect = \InternalProgram @effect -> effect