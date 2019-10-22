(*****************************************)
(*               Type                    *)


type 'a tree  = Empty | Node  of  ('a* 'a tree * 'a tree)

                               
(*****************************************)
(*             Primitive                 *)
let create n=Node(n,Empty,Empty)
           
let rec insert abr v =
  match abr with
  |Empty->create v
  |Node (x,fg,fd)->if(x>v)then
                     Node (x,(insert fg v),fd)
                   else
                     Node (x,fg,(insert fd v))
                 
                 
(*exerice 1*) 
(* fonction qui retourne l'elements a l'indice i *)
let rec get l i=
  match l with
  |[]->failwith "list vide"
  |x::xs->if(i=0)then
           x
         else
           get xs (i-1)

(*fonction qui enleve l'element i de la list*)
let rec remove l x =
  match l with
  |[]->[]
  |v::vs->if(v==x)then
            vs
          else
            v::(remove vs x)

(* Premiere fonction : extraction_alea
   prend deux lists L et P
   choisit un element parmis L 
   l'empile en tete de P 
   et refait l'operation jusqu'a ce que L est vide 

 *)
let extraction_alea l p =
  let t=(List.length l) in
    let x=get l (Random.int  t)in
          let nvList=(remove l x)in
          let nacc=x::p in
          (nvList,nacc)
  
(*fonction qui cree une liste trier appartir d'un nombre n *)
let rec create_List n =
  if(n==0)then
    []
  else
    n::(create_List (n-1))

(* fonction gen_permutation *)
let  gen_permutation n=
  let lis=create_List n in
  let rec loop (l,p)=
    match l with
    |[]->p
    |ls->let (n,v)= extraction_alea l p in
         loop (n,v)
  in
  loop (lis,[])
(*exercice 2*)

let listetoABR l =
  let rec loop liste acc =
    match liste with
    |[]->acc
    |x::xs-> loop xs (insert acc x)
  in
  loop l Empty

(* Question 2.4 : application de fonction de definition de l'arbre avec 
   o(A) ="" si A est reduit a une feuille 
   o(A)= (o(G))o(D) si A est un  neud interne avec G et D comme c sous-noeuds (fills gauche fils droite)

*)
let rec definitionABR abr=
  match abr with
  |Empty->""
  |Node (_,Empty,Empty)->"()"
  |Node (_,fg,fd) ->"("^definitionABR fg^")"^(definitionABR fd)


