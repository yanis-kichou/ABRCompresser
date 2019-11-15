(*****************************************)
(*               Type                    *)


type 'a tree  = Empty | Node  of  ('a* 'a tree * 'a tree)

                               
(*****************************************)
(*             Primitive                 *)
let create n=Node(n,Empty,Empty)

let  rec insertABR ab v =
   match ab with
    |Empty->create (v)
    |Node (x,fg,fd)->if(x>v)then
                           Node (x,(insertABR fg v ),fd)
                         else
                           Node (x,fg,(insertABR fd v ))
  
           
           
let  insert ab v =
  let rec loop abr pere= 
    match abr with
    |Empty->create (pere,v)
    |Node ((a,x),fg,fd)->if(x>v)then
                           Node ((a,x),(loop fg  x),fd)
                         else
                           Node ((a,x),fg,(loop fd  x))
  in
  loop ab 0
let  insert ab v =
  let rec loop abr pere= 
    match abr with
    |Empty->create (pere,v)
    |Node ((a,x),fg,fd)->if(x>v)then
                           Node ((a,x),(loop fg  x),fd)
                         else
                           Node ((a,x),fg,(loop fd  x))
  in
  loop ab 0
                                     
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

let listetoABRCompresser l =
  let rec loop liste acc =
    match liste with
    |[]->acc
    |x::xs-> loop xs (insert acc x)
  in
  loop l Empty
  
let listetoABR l =
  let rec loop liste acc =
    match liste with
    |[]->acc
    |x::xs-> loop xs (insertABR acc x)
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




                  
(*Question 2.5*)
let rec comp ab v =
  match ab with
  |Empty->Empty
  |Node(a,fg,fd)->match v with
                  |Empty->Node([a],comp fg v,comp fd v)
                  |Node(va,g,d)->Node(a::va,comp fg g,comp fd d)
                               
  
let rec ajout ab l =
  match l with
  |[]->[((definitionABR ab),comp ab Empty)]
  |(str,v)::ls->if(String.equal str (definitionABR ab))then
                  (str,(comp ab v ))::ls
                else
                  (str,v)::(ajout ab ls)
let compresse abr=
  let rec loop ab acc=
    match ab with
    |Empty->acc
    |Node(a,g,d)->let nva =loop g acc in
                  let nvd= loop d nva in
                  ajout ab nvd
  in
  loop abr []
let rec mise_jour l =
  match l with
  |(st,Node(a,g,d))::ls->(st,Node(a,Empty,Empty))::(mise_jour ls)
  |_->[]
    
             
let rec get_fils str li=
  match li with
  |[]->Empty
  |(st,a)::ls->if(String.equal st str)then
                 a
               else
                 (get_fils str ls)

let  compresseABR ab =
  let valu =mise_jour (compresse ab) in
  let rec loop arbre= 
  match arbre with
  |Empty->Empty
  |Node(a,g,d)->match get_fils (definitionABR arbre )valu with
                |Empty->Empty
                |Node(a,fg,fd)->Node(a,loop g,loop d)
  in
  loop ab
    

(* rechercher un element *)
let rec recherche_list valeur liste pere =
  match liste with
  |[]->(pere,-1)
  |(x,y)::sl->if(x==pere )then
                if(y==valeur)then
                (pere,0)
                else
                  if(y<valeur)then
                    (y,2)
                  else
                    (y,1)
              else
                (recherche_list valeur sl pere)

let rec recherche arbre x pere =
  match arbre with
  |Empty -> false
  |Node([],g,d)->false
  |Node(s,Empty,Empty)->List.fold_left (||) false (List.map (fun (a,b)->a==pere && b==x)  s ) 
  |Node(s,g,d)-> match recherche_list x s pere with
                 |(_,0)->true
                 |(p,1)->((print_int p );recherche g x p)
                 |(p,2)->((print_int p );recherche d x p)
                 |(_,-1)-> false
