module Party where

import Data.List
import Data.Tree
import Data.Monoid

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL emps fun) =
  GL (employee:emps) (fun + empFun employee)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL empsA funA) (GL empsB funB) =
    let
      allEmps = empsA `union` empsB
      newFun = foldr (\emp acc -> acc + empFun emp) 0 allEmps
    in GL allEmps newFun

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label forest) =
  f label $ map (treeFold f) forest

nextLevel :: Employee -> [(GuestList, GuestList)]
            -> (GuestList, GuestList)
nextLevel boss [] = let gl = GL [boss] (empFun boss) in (gl, mempty)
nextLevel boss glTupleList =
  (withBoss, withoutBoss)
  where
    -- 不包括 boss，在子树中选择 fun 比较大的那一个
    withoutBoss = foldr (\tuple acc -> acc <> uncurry moreFun tuple)
                        mempty
                        glTupleList
    -- 包括 boss，
    withBoss = glCons boss $
                      foldr (\(GL (subBoss:emps) _, withoutSubBoss) acc ->
                              -- 选择子树中不包括 boss 和包括 boss 但去掉 subBoss 中 fun 比较大的那一个
                              acc <> moreFun withoutSubBoss (GL emps $ foldr (\ emp fun -> fun + empFun emp) 0 emps))
                            mempty
                            glTupleList

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

main :: IO [()]
main = do
  content <- readFile "company.txt"
  let empTree = read content :: Tree Employee
      (GL emps fun) = maxFun empTree
  putStrLn ("Total fun: " ++ show fun)
  mapM (putStrLn . empName) emps
