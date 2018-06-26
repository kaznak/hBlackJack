# hBlackJack

State モナドを使ったバージョン。
StateT で IO も出来るようにしてある。
以下のようにして実行可能。数字はランダムシード。

	stack ghci src/BlackJack.hs
	game 0
	
