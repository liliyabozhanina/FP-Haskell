{- Атака на Цезаровия шифър

Една от основните слабости на Цезаровия шифър се състои в това, че броят на възможните шифри е ограничен до броя на ротациите на буквите 
в азбуката минус едно. Това прави Цезаровия шифър податлив на т. нар. brute force атака, т. е. атака, която генерира всички възможни 
дешифровки на кодираното съобщение.


Задача 1.

Напишете функцията crackall alphabet encrypted, която връща списък от всички възможни дешифровки на кодираното съобщение encrypted.

Пример: 

crackall ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR" -> 

["EXXEGOPSRHSRXSQSVVSAEXXIREQ","DWWDFNORQGRQWRPRUURZDWWHQDP","CVVCEMNQPFQPVQOQTTQYCVVGPCO",

"BUUBDLMPOEPOUPNPSSPXBUUFOBN","ATTACKLONDONTOMORROWATTENAM","ZSSZBJKNMCNMSNLNQQNVZSSDMZL",

"YRRYAIJMLBMLRMKMPPMUYRRCLYK","XQQXZHILKALKQLJLOOLTXQQBKXJ","WPPWYGHKJZKJPKIKNNKSWPPAJWI",

"VOOVXFGJIYJIOJHJMMJRVOOZIVH","UNNUWEFIHXIHNIGILLIQUNNYHUG","TMMTVDEHGWHGMHFHKKHPTMMXGTF",

"SLLSUCDGFVGFLGEGJJGOSLLWFSE","RKKRTBCFEUFEKFDFIIFNRKKVERD","QJJQSABEDTEDJECEHHEMQJJUDQC",

"PIIPRZADCSDCIDBDGGDLPIITCPB","OHHOQYZCBRCBHCACFFCKOHHSBOA","NGGNPXYBAQBAGBZBEEBJNGGRANZ",

"MFFMOWXAZPAZFAYADDAIMFFQZMY","LEELNVWZYOZYEZXZCCZHLEEPYLX","KDDKMUVYXNYXDYWYBBYGKDDOXKW",

"JCCJLTUXWMXWCXVXAAXFJCCNWJV","IBBIKSTWVLWVBWUWZZWEIBBMVIU","HAAHJRSVUKVUAVTVYYVDHAALUHT",

"GZZGIQRUTJUTZUSUXXUCGZZKTGS"]


Задача 2.

След като сме генерирали всички възможни дешифровки, бихме могли лесно да намерим най-вероятните от тях, използвайки факта, че някои 
кратки думи, напр. the, at, on, се срещат много често в английския език. За тази цел най-напред напишете функция substring sub str, 
която проверява дали поднизът sub се среща в низа str.

Примери:

substring "Haskell" "Haskell Curry" -> True

substring "Curry" "Haskell Curry" -> True

substring "Turing" "Haskell Curry" -> False


Задача 3.

Използвайте функциите от предишните две задачи, за да напишете функцията crackcandidates alphabet commonwords encrypted, която 
приема списък с често срещани думи и криптирано съобщение и връща списък с потенициални вероятни разшифровки. Една разшифровка 
се смята за вероятна, ако съдържа поне една от думите от списъка с често срещани думи.

Пример:

crackcandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR" -> ["ATTACKLONDONTOMORROWATTENAM"] -}
