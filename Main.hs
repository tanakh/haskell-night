{-# LANGUAGE RecursiveDo #-}

module Main(main) where

import MonadPoint
import NicoTwitter

--

titlePage title name =
  scale 0.9 $ do
    scaleh 0.65 $ do
      scalehu 0.7 $ do
        holstack $ mapM_ txtc $ lines title
      scalehd 0.2 $ do
        txtc name

subtitle title =
  scale 0.9 $ do
    scaleh 0.3 $ do
      txtc title

tmpl title m =
  page $ do
    scale 0.9 $ do
      scalehu 0.2 $ do
        txtc title
      scalehd 0.75 $ do
        m

--

myPresen = pages $ mdo
  titlePage
    "Haskellerの\nHaskellによる\nHaskellerのためのプレゼン"
    "田中 英行"

  tmpl "自己紹介" $ do
    list $ do
      li "田中英行 (tanakh：はてな、twitterほか)"
      li "Preferred Infrastructure (PFI) 勤務"
      li "Haskell歴"
      ul $ do
        li "2004年ごろ?"
        li "Shu-thing（しょぼいシューティング）"
        li "Monadius手伝ったり"
        li "最近では、Suffix Arrayの構築プログラム\n(であるC++コードを生成するプログラム)を\nHaskellで書いたり"

  subtitle "発端"

  tmpl "HaskellでプレゼンしないHaskellerの人って…" $ do
    list $ do
      li "のようなことを言われる。"
      li "JavaScriptの人は\nJavaScriptでやってる云々"
      li "良しじゃあやったろうじゃないの"
      li "一週間後完成。しかし…           "

  tmpl "残念な結果" $ do
    list $ do
      li "ひどい言われようなのであった…"
      ul $ do
        li "(ScalaじゃなくてHaskellです…)"
    scalehd 0.6 $ scalev 0.6 $ do
      pict "dis.png"

  subtitle "本題"
  
  tmpl "Haskellerのためのプレゼンツール" $ do
    list $ do
      li "Haskellで書かれていて\nHaskellで書く\n誰得プレゼンツール"
      ul $ do
        li "を作りました"
        li "MonadPointと命名"
      li "今動いているこれがそうです"
    return ()

  tmpl "特徴" $ do
    list $ do
      li "モナディックプレゼンコンビネータ"
      ul $ do
        li "プレゼンをDSLとしてHaskellで記述します"
      li "プログラマブル"
      ul $ do
        li "Haskellで書くのでなんでもできます"
      li "微妙なレイアウト機能"
      ul $ do
        li "適当に書くと適当にレイアウトしてくれます"

  tmpl "例(1)" $ do
    scale 0.8 $ txtarea
      [ "myPresen = pages $ mdo"
      , "  titlePage"
      , "    \"Haskellerの\\nHaskellによる\\nHaskellerのためのプレゼン\""
      , "    \"田中 英行\""
      ,  ""
      , "  tmpl \"自己紹介\" $ do"
      , "    list $ do"
      , "      li \"田中英行 (tanakh：はてな、twitterほか)\""
      , "      li \"Preferred Infrastructure (PFI) 勤務\""
      , "      li \"Haskell歴\""
      , "      ul $ do"
      , "        li \"2004年ごろ?\""
      , "  ..."
      ]

  tmpl "例(2)" $ do
    scale 0.8 $ txtarea
      [ "do txtl \"文字\"     -- 文字とか"
      , "   pict \"hoge.jpg\" -- 画像とか"
      , "   box             -- 図形とか"
      , ""
      , "-- 配置のしかた"
      , "holstack $ do -- 縦に並べる"
      , "  txtl \"あいうえお\""
      , "  txtl \"かきくけこ\""
      ,  ""
      ]

  tmpl "いいところ(1)" $ do
    list $ do
      li "ページ間の重複部分を簡単に抽象化できる"
    scalehd 0.8 $ scalevl 0.45 $ txtarea
      ["titlePage title name ="
      , "  scale 0.9 $ do"
      , "    scaleh 0.65 $ do"
      , "      scalehu 0.7 $ do"
      , "        holstack $ mapM_ txtc $ lines title"
      , "      scalehd 0.2 $ do"
      , "        txtc name"
      ]
    scalehd 0.8 $ scalevr 0.45 $ txtarea
      [ "tmpl title m ="
      , "  page $ do"
      , "    scale 0.9 $ do"
      , "      scalehu 0.2 $ do"
      , "        txtc title"
      , "      scalehd 0.75 $ do"
      , "        m"
      ]
  
  tmpl "いいところ(2)" $ do
    list $ li "構造を簡単に抽象化できる"
    scalehd 0.8 $ scalev 0.8 $ txtarea
      [ "scale d $ do"
      , " ... -- 子要素を縮小する"
      , "page $ do"
      , " ... -- 子要素を一つのページにまとめる"
      , "pages $ do"
      , " ... -- 子要素に連番を振り前後移動できるようにする"
      , "list $ do"
      , " ... -- 子要素をプレゼン的なリストのレイアウトに並べる"
      ]

  tmpl "オリジナルな機能" $ do
    list $ do
      li "条件分岐"
      ul $ do
        li "ページ間の移動はページの構成要素の\n一つとして実装されているので、\n好きに付け替え可能"
        li "条件分岐などもできる"
        ul $ do
          li "（何かうれしいのか？）"
      li "プラグイン"
      ul $ do
        li "任意のHaskellコードを追加できる機能"
        li "ニコニコ動画風コメント機能"
        ul $ do
          li "実演中 (#hogehoge)"

  tmpl "今後の予定" $ do
    list $ do
      li "GHC API対応"
      li "コードの自動色づけ"
      li "リッチなアニメーション機能"
      li "描画周りの改善"
      ul $ do
        li "GLUT捨てたい"
        li "アンチエイリアスかけたい"
      li "レポジトリ(建設予定地)"
      ul $ do
        li "http://github.com/tanakh/MonadPoint"

  subtitle "ご清聴ありがとうございました"

  return ()


myPresen' = do
  nico <- nicoTwitter "%23hogehoge"
  plugin nico
  myPresen

--

cfg = Config "./dat" "ARISAKA.ttf" "ARISAKA_fix.ttf"

main :: IO ()
main = runPresentation cfg myPresen'
