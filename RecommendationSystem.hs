import System.Random
import System.IO.Unsafe
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory = [("user1",[["item1", "item2","item3"],[ "item1", "item2" , "item4"]]),("user2", [["item2","item5"],["item4","item5"]]),("user3", [["item3","item2"]]) ,("user4", [] )]


createEmptyFreqList [] = []
createEmptyFreqList (x:xs) =  (x,[]):createEmptyFreqList xs

getAllUsersStats []= []
getAllUsersStats ((user,[]):restusers)= (getAllUsersStatshelper items ((user,[]))):getAllUsersStats restusers

getAllUsersStats ((user,((firstitem:restitems):restcarts)):restusers)= (getAllUsersStatshelper items ((user,((firstitem:restitems):restcarts)))):getAllUsersStats restusers

getAllUsersStatshelper items ((user,[])) = (user,helperNew items ([]))
getAllUsersStatshelper items ((user,((firstitem:restitems):restcarts))) = (user,helperNew items ((firstitem:restitems):restcarts))

helperNew [] _ = [] 
helperNew (item:xs) [] = (item,[]): helperNew xs []
helperNew (item:xs) ((firstitem:restitems):restcarts)| helperInt item ((firstitem:restitems):restcarts)==0 = (item,[]):helperNew xs ((firstitem:restitems):restcarts)
                                                     | otherwise= (item, helperNew4 (helperNew2 item ((firstitem:restitems):restcarts)) ): helperNew xs ((firstitem:restitems):restcarts)
helperNew4 [] = []
helperNew4 ((item,freq):xs) = helperNew5 ((item,freq)) xs : helperNew4 (remove1 ((item,freq)) xs)

remove1 _ []=[]
remove1 (item,x) ((item1,y):xs) | item==item1 = remove1 (item,x) xs
                 | otherwise = (item1,y): remove1 (item,x) xs

helperNew5 ((item,freq)) [] = ((item,freq))
helperNew5 ((item,freq)) ((item1,freq1):xs) | item==item1 = helperNew5 ((item,(freq+freq1))) xs
                                            | otherwise = helperNew5 ((item,freq)) xs

helperNew2 _ []=[]
helperNew2 item ((firstitem:restitems):restcarts) | founded item (firstitem:restitems) = helperNew3 item (firstitem:restitems) ++ helperNew2 item restcarts 
                                                  | otherwise = helperNew2 item restcarts
helperNew3 _ [] = []
helperNew3 item (firstitem:restitems) | item==firstitem = helperNew3 item restitems
                                      | otherwise = (firstitem,1): helperNew3 item restitems
founded _ [] = False
founded item (firstitem:restitems) | item==firstitem = True
                                   | otherwise = founded item restitems

helperInt x [] = 0
helperInt x ([]:restcarts) = (helperInt x restcarts)
helperInt x ((firstitem:restitems):restcarts) | x==firstitem = 1 + (helperInt x restcarts)
                                              | otherwise = (helperInt x (restitems:restcarts))

purchasesIntersection _ [] = []
purchasesIntersection ((item,[]):ys) ((user,((item1,[]):ys1)):users)= intersectionHelper ((item,[]):ys) ((item1,[]):ys1): purchasesIntersection ((item,[]):ys) users
purchasesIntersection ((item,[]):ys) ((user,((item1,((otheritem1,freq1):xs1)):ys1)):users) = intersectionHelper ((item,[]):ys) ((item1,((otheritem1,freq1):xs1)):ys1):purchasesIntersection ((item,[]):ys) users
purchasesIntersection ((item,((otheritem,freq):xs)):ys) ((user,((item1,[]):ys1)):users) = intersectionHelper ((item,((otheritem,freq):xs)):ys) ((item1,[]):ys1):purchasesIntersection ((item,((otheritem,freq):xs)):ys) users
purchasesIntersection ((item,((otheritem,freq):xs)):ys) ((user,((item1,((otheritem1,freq1):xs1)):ys1)):users) = intersectionHelper ((item,((otheritem,freq):xs)):ys) ((item1,((otheritem1,freq1):xs1)):ys1):purchasesIntersection ((item,((otheritem,freq):xs)):ys) users

intersectionHelper [] [] = []
intersectionHelper ((item,[]):ys) ((item1,[]):ys1)= intersectionHelper ys ys1
intersectionHelper ((item,[]):ys) ((item1,_):ys1) = intersectionHelper ys ys1
intersectionHelper ((item,_):ys) ((item1,[]):ys1) = intersectionHelper ys ys1
intersectionHelper ((item,((otheritem,freq):xs)):ys) ((item1,((otheritem1,freq1):xs1)):ys1) = (item,helperNew4 (intersectionHelper1 ((otheritem,freq):xs) ((otheritem1,freq1):xs1)) ): intersectionHelper ys ys1

intersectionHelper1 x y = y++x
freqListUsers user = helperNew4 (freqListUsersHelper (freqListUsersExtract user))

freqListUsersHelper [[]]=[]
freqListUsersHelper ([]:ys) =[]
freqListUsersHelper (((item,x):xs):ys)= (freqListUsersHelper ys)++(freqListUsersHelper1 ((item,x):xs)) 
freqListUsersHelper1 []=[]
freqListUsersHelper1 ((item,x):xs) = x++(freqListUsersHelper1 xs)

freqListUsersExtract user = purchasesIntersection (freqListUsersExHelper1  user (getAllUsersStats purchasesHistory)) (freqListUsersExHelper user (getAllUsersStats purchasesHistory))

freqListUsersExHelper _ []=[]
freqListUsersExHelper user ((user1,x):xs)| user==user1 = xs
                                       | otherwise = (user1,x): freqListUsersExHelper user xs
freqListUsersExHelper1 _ []=[]
freqListUsersExHelper1 user ((user1,x):xs)| user==user1 = x
                                        | otherwise = freqListUsersExHelper1 user xs
   
recommendBasedOnUsers user | (recommendBasedOnUsersHelper (freqListUsers user))==[] = ""
                           | otherwise= (recommendBasedOnUsersHelper (freqListUsers user) ) !! randomZeroToX(length(recommendBasedOnUsersHelper (freqListUsers user))-1)                                


recommendBasedOnUsersHelper [] = []
recommendBasedOnUsersHelper ((item,x):xs)= (recHelper x item)++(recommendBasedOnUsersHelper xs)

recHelper 0 _ = []
recHelper x item = item:recHelper (x-1) item


freqListItems user= freqListItemsHelper (freqListItemsHelper1 user (getAllUsersStats purchasesHistory))

freqListItemsHelper [] = []
freqListItemsHelper ((item1,[]):ys1) = freqListItemsHelper ys1
freqListItemsHelper ((item1,((otheritem1,freq1):xs1)):ys1) = (item1, sum (change ((otheritem1,freq1):xs1))): freqListItemsHelper ys1

freqListItemsHelper1 user ((user1,((item1,[]):ys1)):users)| user==user1 = ((item1,[]):ys1)
                                                          | otherwise = (freqListItemsHelper1 user users)

freqListItemsHelper1 user ((user1,((item1,((otheritem1,freq1):xs1)):ys1)):users)| user==user1 = ((item1,((otheritem1,freq1):xs1)):ys1)
                                                                               | otherwise = (freqListItemsHelper1 user users)

change [] = []
change ((otheritem1,freq1):xs1) = freq1: change xs1 

freqListCart user cart = helperNew4 (freqListCartHelper user cart)
freqListCartHelper _ [] = []
freqListCartHelper user (i:is)=  getList i (freqListItemsHelper1 user (getAllUsersStats purchasesHistory)) ++ freqListCartHelper user is

getList i  ((item1,[]):ys1) | i==item1 = []
                            | otherwise = getList i ys1
getList i ((item1,((otheritem1,freq1):xs1)):ys1) | i==item1 = ((otheritem1,freq1):xs1)
                                                 | otherwise = getList i ys1

freqListCartAndItems user cart = helperNew4 ((freqListItems user)++(freqListCart user cart))

recommendEmptyCart user | (recommendBasedOnUsersHelper (freqListItems user))==[] = ""
                        | otherwise= (recommendBasedOnUsersHelper (freqListItems user) ) !! randomZeroToX(length(recommendBasedOnUsersHelper (freqListItems user))-1)                                


recommendBasedOnItemsInCart user cart| (recommendBasedOnUsersHelper (freqListCartAndItems user cart))==[] = ""
                                     | otherwise= (recommendBasedOnUsersHelper (freqListCartAndItems user cart) ) !! randomZeroToX(length(recommendBasedOnUsersHelper (freqListCartAndItems user cart))-1)                                

recommend user cart | recommendHelper user cart=="" = reccomendfromitems items
                    | otherwise = recommendHelper user cart



recommendHelper user cart | cart==[] = if  randomZeroToX(1)==0 then (recommendEmptyCart user) else (recommendBasedOnUsers user)
                          | otherwise = if  randomZeroToX(1)==0 then (recommendBasedOnItemsInCart user cart) else (recommendBasedOnUsers user)


reccomendfromitems items = items !! randomZeroToX(length(items)-1)
