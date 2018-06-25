# hBlackJack

IORef を駆使したバージョン。
"stack ghci src/BlackJack.hs" で読み込み、
"game 0" などでプレイ。番号はシード。

ほとんど命令形で書けるようになるが、
IORef がどのコンテキストに載っているか気にする必要がある。
