# hBlackJack

State モナドを使ったバージョン。
StateT で IO も出来るようにしてある。

## 使い方
以下のようにして実行。数字はランダムシード。

~~~
stack ghci src/BlackJack.hs
game 0
~~~	
