# META
~~~ini
description=Multiline string with many lines (regression test for stack overflow, #9464)
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [data] { pf: platform "./platform.roc" }

data =
    \\Line 000
    \\Line 001
    \\Line 002
    \\Line 003
    \\Line 004
    \\Line 005
    \\Line 006
    \\Line 007
    \\Line 008
    \\Line 009
    \\Line 010
    \\Line 011
    \\Line 012
    \\Line 013
    \\Line 014
    \\Line 015
    \\Line 016
    \\Line 017
    \\Line 018
    \\Line 019
    \\Line 020
    \\Line 021
    \\Line 022
    \\Line 023
    \\Line 024
    \\Line 025
    \\Line 026
    \\Line 027
    \\Line 028
    \\Line 029
    \\Line 030
    \\Line 031
    \\Line 032
    \\Line 033
    \\Line 034
    \\Line 035
    \\Line 036
    \\Line 037
    \\Line 038
    \\Line 039
    \\Line 040
    \\Line 041
    \\Line 042
    \\Line 043
    \\Line 044
    \\Line 045
    \\Line 046
    \\Line 047
    \\Line 048
    \\Line 049
    \\Line 050
    \\Line 051
    \\Line 052
    \\Line 053
    \\Line 054
    \\Line 055
    \\Line 056
    \\Line 057
    \\Line 058
    \\Line 059
    \\Line 060
    \\Line 061
    \\Line 062
    \\Line 063
    \\Line 064
    \\Line 065
    \\Line 066
    \\Line 067
    \\Line 068
    \\Line 069
    \\Line 070
    \\Line 071
    \\Line 072
    \\Line 073
    \\Line 074
    \\Line 075
    \\Line 076
    \\Line 077
    \\Line 078
    \\Line 079
    \\Line 080
    \\Line 081
    \\Line 082
    \\Line 083
    \\Line 084
    \\Line 085
    \\Line 086
    \\Line 087
    \\Line 088
    \\Line 089
    \\Line 090
    \\Line 091
    \\Line 092
    \\Line 093
    \\Line 094
    \\Line 095
    \\Line 096
    \\Line 097
    \\Line 098
    \\Line 099
    \\Line 100
    \\Line 101
    \\Line 102
    \\Line 103
    \\Line 104
    \\Line 105
    \\Line 106
    \\Line 107
    \\Line 108
    \\Line 109
    \\Line 110
    \\Line 111
    \\Line 112
    \\Line 113
    \\Line 114
    \\Line 115
    \\Line 116
    \\Line 117
    \\Line 118
    \\Line 119
    \\Line 120
    \\Line 121
    \\Line 122
    \\Line 123
    \\Line 124
    \\Line 125
    \\Line 126
    \\Line 127
    \\Line 128
    \\Line 129
    \\Line 130
    \\Line 131
    \\Line 132
    \\Line 133
    \\Line 134
    \\Line 135
    \\Line 136
    \\Line 137
    \\Line 138
    \\Line 139
    \\Line 140
    \\Line 141
    \\Line 142
    \\Line 143
    \\Line 144
    \\Line 145
    \\Line 146
    \\Line 147
    \\Line 148
    \\Line 149
    \\Line 150
    \\Line 151
    \\Line 152
    \\Line 153
    \\Line 154
    \\Line 155
    \\Line 156
    \\Line 157
    \\Line 158
    \\Line 159
    \\Line 160
    \\Line 161
    \\Line 162
    \\Line 163
    \\Line 164
    \\Line 165
    \\Line 166
    \\Line 167
    \\Line 168
    \\Line 169
    \\Line 170
    \\Line 171
    \\Line 172
    \\Line 173
    \\Line 174
    \\Line 175
    \\Line 176
    \\Line 177
    \\Line 178
    \\Line 179
    \\Line 180
    \\Line 181
    \\Line 182
    \\Line 183
    \\Line 184
    \\Line 185
    \\Line 186
    \\Line 187
    \\Line 188
    \\Line 189
    \\Line 190
    \\Line 191
    \\Line 192
    \\Line 193
    \\Line 194
    \\Line 195
    \\Line 196
    \\Line 197
    \\Line 198
    \\Line 199
    \\Line 200
    \\Line 201
    \\Line 202
    \\Line 203
    \\Line 204
    \\Line 205
    \\Line 206
    \\Line 207
    \\Line 208
    \\Line 209
    \\Line 210
    \\Line 211
    \\Line 212
    \\Line 213
    \\Line 214
    \\Line 215
    \\Line 216
    \\Line 217
    \\Line 218
    \\Line 219
    \\Line 220
    \\Line 221
    \\Line 222
    \\Line 223
    \\Line 224
    \\Line 225
    \\Line 226
    \\Line 227
    \\Line 228
    \\Line 229
    \\Line 230
    \\Line 231
    \\Line 232
    \\Line 233
    \\Line 234
    \\Line 235
    \\Line 236
    \\Line 237
    \\Line 238
    \\Line 239
    \\Line 240
    \\Line 241
    \\Line 242
    \\Line 243
    \\Line 244
    \\Line 245
    \\Line 246
    \\Line 247
    \\Line 248
    \\Line 249
    \\Line 250
    \\Line 251
    \\Line 252
    \\Line 253
    \\Line 254
    \\Line 255
    \\Line 256
    \\Line 257
    \\Line 258
    \\Line 259
    \\Line 260
    \\Line 261
    \\Line 262
    \\Line 263
    \\Line 264
    \\Line 265
    \\Line 266
    \\Line 267
    \\Line 268
    \\Line 269
    \\Line 270
    \\Line 271
    \\Line 272
    \\Line 273
    \\Line 274
    \\Line 275
    \\Line 276
    \\Line 277
    \\Line 278
    \\Line 279
    \\Line 280
    \\Line 281
    \\Line 282
    \\Line 283
    \\Line 284
    \\Line 285
    \\Line 286
    \\Line 287
    \\Line 288
    \\Line 289
    \\Line 290
    \\Line 291
    \\Line 292
    \\Line 293
    \\Line 294
    \\Line 295
    \\Line 296
    \\Line 297
    \\Line 298
    \\Line 299
~~~
## platform.roc
~~~roc
platform ""
    requires {} { data : Str }
    exposes []
    packages {}
    provides { "roc_data": data_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

data_for_host : Str
data_for_host = data
~~~
# MONO
~~~roc
# platform
data_for_host = <required>

# app
data = "Line 000\nLine 001\nLine 002\nLine 003\nLine 004\nLine 005\nLine 006\nLine 007\nLine 008\nLine 009\nLine 010\nLine 011\nLine 012\nLine 013\nLine 014\nLine 015\nLine 016\nLine 017\nLine 018\nLine 019\nLine 020\nLine 021\nLine 022\nLine 023\nLine 024\nLine 025\nLine 026\nLine 027\nLine 028\nLine 029\nLine 030\nLine 031\nLine 032\nLine 033\nLine 034\nLine 035\nLine 036\nLine 037\nLine 038\nLine 039\nLine 040\nLine 041\nLine 042\nLine 043\nLine 044\nLine 045\nLine 046\nLine 047\nLine 048\nLine 049\nLine 050\nLine 051\nLine 052\nLine 053\nLine 054\nLine 055\nLine 056\nLine 057\nLine 058\nLine 059\nLine 060\nLine 061\nLine 062\nLine 063\nLine 064\nLine 065\nLine 066\nLine 067\nLine 068\nLine 069\nLine 070\nLine 071\nLine 072\nLine 073\nLine 074\nLine 075\nLine 076\nLine 077\nLine 078\nLine 079\nLine 080\nLine 081\nLine 082\nLine 083\nLine 084\nLine 085\nLine 086\nLine 087\nLine 088\nLine 089\nLine 090\nLine 091\nLine 092\nLine 093\nLine 094\nLine 095\nLine 096\nLine 097\nLine 098\nLine 099\nLine 100\nLine 101\nLine 102\nLine 103\nLine 104\nLine 105\nLine 106\nLine 107\nLine 108\nLine 109\nLine 110\nLine 111\nLine 112\nLine 113\nLine 114\nLine 115\nLine 116\nLine 117\nLine 118\nLine 119\nLine 120\nLine 121\nLine 122\nLine 123\nLine 124\nLine 125\nLine 126\nLine 127\nLine 128\nLine 129\nLine 130\nLine 131\nLine 132\nLine 133\nLine 134\nLine 135\nLine 136\nLine 137\nLine 138\nLine 139\nLine 140\nLine 141\nLine 142\nLine 143\nLine 144\nLine 145\nLine 146\nLine 147\nLine 148\nLine 149\nLine 150\nLine 151\nLine 152\nLine 153\nLine 154\nLine 155\nLine 156\nLine 157\nLine 158\nLine 159\nLine 160\nLine 161\nLine 162\nLine 163\nLine 164\nLine 165\nLine 166\nLine 167\nLine 168\nLine 169\nLine 170\nLine 171\nLine 172\nLine 173\nLine 174\nLine 175\nLine 176\nLine 177\nLine 178\nLine 179\nLine 180\nLine 181\nLine 182\nLine 183\nLine 184\nLine 185\nLine 186\nLine 187\nLine 188\nLine 189\nLine 190\nLine 191\nLine 192\nLine 193\nLine 194\nLine 195\nLine 196\nLine 197\nLine 198\nLine 199\nLine 200\nLine 201\nLine 202\nLine 203\nLine 204\nLine 205\nLine 206\nLine 207\nLine 208\nLine 209\nLine 210\nLine 211\nLine 212\nLine 213\nLine 214\nLine 215\nLine 216\nLine 217\nLine 218\nLine 219\nLine 220\nLine 221\nLine 222\nLine 223\nLine 224\nLine 225\nLine 226\nLine 227\nLine 228\nLine 229\nLine 230\nLine 231\nLine 232\nLine 233\nLine 234\nLine 235\nLine 236\nLine 237\nLine 238\nLine 239\nLine 240\nLine 241\nLine 242\nLine 243\nLine 244\nLine 245\nLine 246\nLine 247\nLine 248\nLine 249\nLine 250\nLine 251\nLine 252\nLine 253\nLine 254\nLine 255\nLine 256\nLine 257\nLine 258\nLine 259\nLine 260\nLine 261\nLine 262\nLine 263\nLine 264\nLine 265\nLine 266\nLine 267\nLine 268\nLine 269\nLine 270\nLine 271\nLine 272\nLine 273\nLine 274\nLine 275\nLine 276\nLine 277\nLine 278\nLine 279\nLine 280\nLine 281\nLine 282\nLine 283\nLine 284\nLine 285\nLine 286\nLine 287\nLine 288\nLine 289\nLine 290\nLine 291\nLine 292\nLine 293\nLine 294\nLine 295\nLine 296\nLine 297\nLine 298\nLine 299"

~~~
# DEV OUTPUT
~~~ini
x64mac=a899ee7b69c42a450342c554780de63a818f59f1a2aaed93526763e502b0e1cf
x64win=7fb522a542530461ff870906593a5b720ce71124488693a2ee097d4e3357c02f
x64mingw=7fb522a542530461ff870906593a5b720ce71124488693a2ee097d4e3357c02f
x64freebsd=284b34f23544bf37db207c622431c861080620e11242208752b49b9dba884c84
x64openbsd=ba10549d2334b1e27bbd4e06bf2edb8fe06c137fa92ec94796af098576465084
x64netbsd=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64musl=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64glibc=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64linux=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64elf=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64v1mac=a899ee7b69c42a450342c554780de63a818f59f1a2aaed93526763e502b0e1cf
x64v1win=7fb522a542530461ff870906593a5b720ce71124488693a2ee097d4e3357c02f
x64v1mingw=7fb522a542530461ff870906593a5b720ce71124488693a2ee097d4e3357c02f
x64v1freebsd=284b34f23544bf37db207c622431c861080620e11242208752b49b9dba884c84
x64v1openbsd=ba10549d2334b1e27bbd4e06bf2edb8fe06c137fa92ec94796af098576465084
x64v1netbsd=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64v1musl=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64v1glibc=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64v1linux=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
x64v1elf=c09edb519329b419dabae95ef49d916bb51d48088106481455a0aa8bce84b84e
arm64mac=f6602e2936ab3c08c2313f8acb0e51d778c945ad3691968e27ce034fe330b01c
arm64win=b1b892999409c0a175dcf9720070480ae83e644eeb645de4dddff14ed5e72739
arm64mingw=b1b892999409c0a175dcf9720070480ae83e644eeb645de4dddff14ed5e72739
arm64linux=da9389a4123b648f2f7f6e195b5c5121b3ca424a67c013a71452c487a80af3b8
arm64musl=da9389a4123b648f2f7f6e195b5c5121b3ca424a67c013a71452c487a80af3b8
arm64glibc=da9389a4123b648f2f7f6e195b5c5121b3ca424a67c013a71452c487a80af3b8
arm64v1win=b1b892999409c0a175dcf9720070480ae83e644eeb645de4dddff14ed5e72739
arm64v1mingw=b1b892999409c0a175dcf9720070480ae83e644eeb645de4dddff14ed5e72739
arm64v1linux=da9389a4123b648f2f7f6e195b5c5121b3ca424a67c013a71452c487a80af3b8
arm64v1musl=da9389a4123b648f2f7f6e195b5c5121b3ca424a67c013a71452c487a80af3b8
arm64v1glibc=da9389a4123b648f2f7f6e195b5c5121b3ca424a67c013a71452c487a80af3b8
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
