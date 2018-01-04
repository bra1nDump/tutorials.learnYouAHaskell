type Name = String
type Data = String
data Item =
  Folder Name [Item]
  | File Name Data
  deriving (Show)

data BreadCrumb = BreadCrumb Name [Item] [Item]
  deriving (Show)
type Zipper = (Item, [BreadCrumb])

focus :: Name -> Zipper -> Zipper
focus targetScopeName (Folder name items, trace) = let
  (preceding, item:following) = break (hasName targetScopeName) items
  in (item, BreadCrumb name preceding following:trace)
  where
    hasName itemName (Folder name _) = itemName == name
    hasName itemName (File name _) = itemName == name

unfocus :: Zipper -> Zipper
unfocus (item, BreadCrumb name preceding following:xs) =
  (Folder name (preceding ++ [item] ++ following), xs)

root = Folder "root" [
    Folder "documents" [
        File "lolza.txt" "yeee, i am lolza text",
        File "pider.txt" "yeee, i am pider text",
        Folder "porn" [
            File "canIGoNowQuestionMark.txt"
              "*filming a guy banging his wife*",
            File "dogTresome.txt"
              "*weird shit going on in here*"
          ]
      ],
    Folder "music" [
        File "lilPip.funeralSong.mp3"
          "I fucked your dead bitch"
      ]
  ]

(-:) x f = f x

porn = (root, []) -: focus "documents" -: focus "porn"
