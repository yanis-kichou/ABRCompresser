(*

Les Binomes: 
1- Aourtilane Khaled 
2- Galou Arezki
3- Kichou Yanis 

 *)
(* Exercice 1 *)

(*  Question 1  *)

(* fonction qui retourne l'elements a l'indice i *)
let rec recupere l i=
  match l with
  |[]->failwith "list vide"
  |x::xs->if(i=0)then
           x
         else
           recupere xs (i-1)
        
(*fonction qui enleve l'element i de la list*)
let rec supprimer l x =
  match l with
  |[]->[]
  |v::vs->if(v==x)then
            vs
          else
            v::(supprimer vs x)

(* Fonction  extraction_alea 
   prend deux lists L et P
   choisit un element parmis L 
   l'empile en tete de P 
   renvoie un couple (L,P) 
 *)
        
let extraction_alea l p =
  let t=(List.length l) in
    let x=recupere l (Random.int  t)in
          let newL=(supprimer l x)in
          let newP=x::p in
          (newL,newP)


(*fonction qui cree une liste trier appartir d'un nombre n *)
let rec creation_liste n =
  if(n==0)then
    []
  else
    n::(creation_liste (n-1))

(* fonction gen_permutation  qui genere une liste aléatoire issu d'un entier  n *)
let  gen_permutation n=
  let liste=creation_liste n in
  let rec loop (l,p)=
    match l with
    |[]->p
    |ls->let (newL,newP)= extraction_alea l p in
         loop (newL,newP)
  in
  loop(liste,[])


(*Exercice 2*)
(* Arbre Binaire de recherche *)

(*==================== Les types ======================*)
  
type 'a tree  = Empty | Node  of  ('a* 'a tree * 'a tree)

(*==================== les primitives =================*)

(* Creation d'une Feuille vide *)
let feuille n= Node(n,Empty,Empty)


(* insertions dans un arbre Binaire de recherche *)
let rec insertion arbre valeur =
  match arbre with 
    |Empty->feuille(valeur)
    |Node (x,fg,fd)->if(x>valeur)then
                           Node (x,(insertion fg valeur ),fd)
                         else
                           Node (x,fg,(insertion fd valeur))
                   
(* fonction max qui retourne le maximum entre deux entier *)
let max a b =
  if(a>b)then
    a
  else
    b

(*
 fonction qui prend en parametre un arbre binaire de recherche  et renvoie sa hauteur 
*)
let rec hauteur arbre =
  match arbre with
  |Empty->0
  |Node(_,fg,fd)->1+ (max (hauteur fg) (hauteur fd))
                        
(* fonction qui calcule le nombre de noeud d'un arbre binaire de recherche *)
let rec nombreNoeud arbre =
  match arbre with
  |Empty->0
  |Node(_,fg,fd)->1+(nombreNoeud fg)+(nombreNoeud fd)


(*
fonction qui prend une liste  et genere l'arbre binaire de recherche
   associer a cette liste qui decrite sont parcours prefix 
 *)
let listetoABR liste =
  let rec loop l acc =
    match l with
    |[]->acc
    |x::xs-> loop xs (insertion acc x)
  in
  loop liste Empty

(* fonction qui recherche  si la valeur v existe dans l'arbre binaire de recheche arbre *)
let rec rechercher arbre v =
  match arbre with
  |Empty -> false
  |Node(clef,fg,fd)->if(clef==v)then
                       true
                     else
                       if(clef>v)then
                         rechercher fg v
                       else
                         rechercher fd v
(* Test sur les arbre binaire de recherche *)
let test =
  let liste =[4;2;3;8;1;9;6;7;5] in
  let arbre= listetoABR liste in
  rechercher arbre 5


(*
 Question 2 

Compression de l'arbre binaire de recherche 
*)

let rec definitionABR abr=
  match abr with
  |Empty->""
  |Node (_,Empty,Empty)->"()"
  |Node (_,fg,fd) ->"("^definitionABR fg^")"^(definitionABR fd)

(*
   test de la fonction definitionABR avec l'exemple de l'enoncé 
*) 
let test=
  let liste =[4;2;3;8;1;9;6;7;5] in
  let arbre= listetoABR liste in
  String.equal "((())())((())())()" (definitionABR arbre)
(* fonction qui inser v dans l'arbre binaire de recherche compressé ab *)
let  insert ab v =
  let rec loop abr pere= 
    match abr with
    |Empty->feuille (pere,v)
    |Node ((a,x),fg,fd)->if(x>v)then
                           Node ((a,x),(loop fg  x),fd)
                         else
                           Node ((a,x),fg,(loop fd  x))
  in
  loop ab 0



(* fonction qui comptes le nombres de noeud interne a un arbre Compresse *)
let  nombreNoeudCompresser arbre =
  let rec loop abr acc=
  match abr with
  |Empty->acc
  |Node(_,fg,fd)->if(List.mem (definitionABR abr) acc)then
                    (loop fg (loop fd acc))
                  else
                    (loop fg (loop fd  ((definitionABR abr )::acc)))
  in
  List.length (loop arbre [])
(*
fonction qui prend un arbre binaire nomale et le concatene avec l'arbre compresser v 
tel que ab et v ont la meme definition de structure

{ definitionABR(ab)=definitionABR(v) } ou { o(ab) == o(v) } 
 *)
let rec assemblerDeuxArbre ab v =
  match ab with
  |Empty->Empty
  |Node(a,fg,fd)->match v with
                  |Empty->Node([a],assemblerDeuxArbre fg v,assemblerDeuxArbre fd v)
                  |Node(va,g,d)->Node(a::va,assemblerDeuxArbre fg g,assemblerDeuxArbre fd d)
                               
(*
  fonction qui ajoute l'arbre ab a la liste des (couple,structure) (definitionABR,structure) 
*)
let rec ajouter_ABR_a_strucuture ab l =
  match l with
  |[]->[((definitionABR ab),assemblerDeuxArbre ab Empty)]
  |(str,v)::ls->if(String.equal str (definitionABR ab))then
                  (str,(assemblerDeuxArbre ab v ))::ls
                else
                  (str,v)::(ajouter_ABR_a_strucuture ab ls)
(*
  fonction qui parcours l'arbre abr et associe chaque sous-arbre a sa bonne structure  dans l'accumulateur 
  qui contient des couples (definitionABR,structure)
*)


let pre_compression arbre=
  let rec loop abr acc=
    match abr with
    |Empty->acc
    |Node(v,fg,fd)->let nvg =loop fg acc in
                  let nvd= loop fd nvg in
                   ajouter_ABR_a_strucuture abr nvd 
  in
  loop arbre []


    
(* fonction qui prend une liste d'arbre et qui retourne une liste de  racine de chaque arbre *)
let rec mise_jour liste =
  match liste with
  |(st,Node(a,g,d))::ls->(st,Node(a,Empty,Empty))::(mise_jour ls)
  |_->[]
    

(*
  fonction qui prend un string str "qui est la defintion d'un arbre avec les fonction o() " et une liste  de couple 
  et renvoie la structure qui a la definition str avec o() 
*)
let rec recuperer_fils str li=
  match li with
  |[]->Empty
  |(st,a)::ls->if(String.equal st str)then
                 a
               else
                 (recuperer_fils str ls)


(* 
   fonction qui prend une liste et retourne un arbre binaire de recherche avec les clefs des noeuds qui sont des couples (pere,valeur)
   ou la racine a comme pere 0 
*)
let listetoABRCompresser l =
  let rec loop liste acc =
    match liste with
    |[]->acc
    |x::xs-> loop xs (insert acc x)
  in
  loop l Empty
             
(*
  fonction principale compresser abr qui prend un arbre binaire de recherche et renvoie un arbre bonaire de recherche compresser 
  ou les structures ne sont pas dupliquer 
 *)

             
let  compresseABR arbre =
  let liste =mise_jour (pre_compression arbre) in
  let _=Gc.full_major() in
  let rec loop arbre= 
  match arbre with
  |Empty->Empty
  |Node(a,g,d)->match recuperer_fils (definitionABR arbre ) liste with
                |Empty->Empty
                |Node(a,fg,fd)->Node(a,loop g,loop d)
  in
  loop arbre
    

  
(* rechercher un element  dans une liste de clefs valeur (Pere,valeur) *)
let  recherche_list valeur liste pere =
  let rec loop l acc=
    match l with 
    |[]->acc
    |(x,v)::xl->if(pere==x)then
                  v::(loop xl acc)
                else
                  loop xl acc
  in
   match loop liste [] with
  |[]->(pere,-1)
  |x::[]->if(x==valeur )then
            (x,0)
          else
            if(x<valeur)then
              (x,2)
            else
              (x,1)
  |x::y::[]->if(x==valeur || y==valeur )then
            (x,0)
          else
            if(x<valeur )then
              (x,2)
            else
              if(y<valeur)then
                (y,2)
              else
               (y,1)
  |_->(pere,-1)
(*
  recherche
*)
let rec recherche_comp arbre x=
  let rec loop abr x pere=
  match abr with
  |Empty -> false
  |Node([],g,d)->false
  |Node(s,Empty,Empty)->List.fold_left (||) false (List.map (fun (a,b)->a==pere && b==x)  s ) 
  |Node(s,g,d)-> match recherche_list x s pere with
                 |(_,0)->true
                 |(p,1)->loop g x p
                 |(p,2)->loop d x p
                 |(_,_)-> false
  in loop arbre x 0



module MyMap=Map.Make(String)
let  insertMap ab v =
  let rec loop abr etiquette= 
    match abr with
    |Empty->let m=MyMap.empty in
            let m=MyMap.add etiquette v m in
            feuille (m)
    |Node (map,fg,fd)->
      let x = MyMap.find etiquette  map in
      if(x>v)then
        Node (map,(loop fg  ((string_of_int x)^etiquette)),fd)
      else
        Node (map,fg,(loop fd ( (string_of_int (x+1))^etiquette)) )
  in
  loop ab  "0"

  
let rec assemblerDeuxArbre_map arbrebinaire arbrecompresser etiquette =
  match arbrebinaire with
  |Empty->Empty
  |Node(valeur,fg,fd)->
    let et=etiquette in
    match arbrecompresser with 
    |Empty->let m=MyMap.empty in
            
            let m=MyMap.add  etiquette  valeur  m in
            Node(m,assemblerDeuxArbre_map fg arbrecompresser et ,assemblerDeuxArbre_map fd arbrecompresser  etiquette)
    |Node(map,g,d)-> Node((MyMap.add  et valeur map),assemblerDeuxArbre_map fg g et  ,assemblerDeuxArbre_map fd d etiquette  )
                      
                      
                   
let rec ajouter_ABR_a_strucuture_map ab l pere =
  match l with
  |[]->[((definitionABR ab),assemblerDeuxArbre_map ab Empty pere)]
  |(str,v)::ls->if(String.equal str (definitionABR ab))then
                  (str,(assemblerDeuxArbre_map ab v pere ))::ls
                else
                  (str,v)::(ajouter_ABR_a_strucuture_map ab ls pere)

let pre_compression_map arbre=
  let rec loop abr acc pere=
    match abr with
    |Empty->acc
    |Node(x,fg,fd)->
      let nvg =loop fg acc  ((string_of_int x)^pere) in
      let nvd= loop fd nvg  ((string_of_int (x+1))^pere)   in
               ajouter_ABR_a_strucuture_map abr nvd pere
      |_->[]
  in
  loop arbre [] "0"
  
let  compresseABR_map arbre =
  let liste =mise_jour (pre_compression_map arbre) in
  
  let _=Gc.compact() in 
  let rec loop abr= 
  match abr with
  |Empty->Empty
  |Node(a,g,d)->match recuperer_fils (definitionABR abr ) liste with
                |Empty->Empty
                |Node(a,fg,fd)->Node(a,loop g,loop d)
  in
  loop arbre

let recherche_map arbre x=
  let rec loop abr x pere=
    match abr with
    |Empty -> false
    |Node(map,g,d)->try 
                     let l=MyMap.find  pere map in
                     if(x==l)then
                       true
                     else
                       if(l<x)then
                       loop d x ((string_of_int (l+1))^pere)
                     else
                       loop g x ((string_of_int l)^pere)
                     with e -> false 
   in loop arbre x "0"
   
let print key password =
    print_string (key ^ " " ^ (string_of_int password) ^ "\n")
let rec parcours arbre =
  match arbre with
  |Empty->print_string("")
  |Node(map,fg,fd)->parcours fg;
                    MyMap.iter print map;
                    parcours fd
                      
   
let checkTime_ABR x=
  let liste =gen_permutation x in
  let arbre =listetoABR liste in
  let debut =Sys.time() in
  let rechercher=rechercher arbre x in
  let fin =Sys.time() in
  fin-.debut

let checkTime_comp x=
  let liste =gen_permutation x in
  let arbre =compresseABR(listetoABRCompresser liste)in
  let debut =Sys.time() in
  let rechercher=recherche_comp arbre x in
  let fin =Sys.time() in
  fin-.debut

let checkTime_map x=
  let liste =gen_permutation x in
  let arbre =compresseABR_map(listetoABR liste)in
  let debut =Sys.time() in
  let rechercher=recherche_map arbre x in
  let fin =Sys.time() in
  fin-.debut

        
let test_recherche_abr str =
  let out = open_out str in
  for i = 1 to 1000 do
    let y = checkTime_ABR i in
    Printf.fprintf out "%d %f\n" i y
  done;
  close_out out
 
let test_recherche_map str =
  let out = open_out str in
  for i = 1 to 1000 do
    let y = checkTime_map i in 
    Printf.fprintf out "%d %f\n" i y 
  done;
  close_out out

let test_recherche_comp str =
  let out = open_out str in
  for i = 1 to 1000 do
    let x = Random.int i in 
    let y = checkTime_comp x in 
    Printf.fprintf out "%d %f\n" i y 
  done;
  close_out out


let test_espace_map str =
  let out = open_out str in
  for i = 1 to 1000 do
    (* Call GC *)
    Gc.compact ();
    (* Get initially allocated bytes *)
    
    (* Allocate something *)
    let arbre =listetoABR (gen_permutation i) in

    let stat=Gc.stat()in
    Gc.compact();
    let init_allocated=Gc.allocated_bytes() in
    Gc.compact();
    let _= compresseABR_map arbre in
    (* Call GC to clean all garbages *)
    Gc.compact ();
    let now_allocated=Gc.allocated_bytes() in
    Printf.fprintf out "%d %f\n" i (now_allocated -. init_allocated)
  done;
  close_out out
    
let test_espace_abr str =
  let out = open_out str in
  for i = 1 to 1000 do
    (* Call GC *)
    Gc.compact ();
    (* Get initially allocated bytes *)
    
    (* Allocate something *)

    let stat=Gc.stat()in
    Gc.compact();
    let init_allocated=Gc.allocated_bytes() in
    Gc.compact();
    let arbre =listetoABR (gen_permutation i) in
    (* Call GC to clean all garbages *)
    Gc.compact ();
    let now_allocated=Gc.allocated_bytes() in
    Printf.fprintf out "%d %f\n" i (now_allocated -. init_allocated)
  done;
  close_out out

let test_espace_liste str =
  let out = open_out str in
  for i = 1 to 1000 do
    (* Call GC *)
    Gc.compact ();
    let arbre =listetoABRCompresser (gen_permutation i) in
    (* Get initially allocated bytes *)
    
    (* Allocate something *)

    let stat=Gc.stat()in
    Gc.compact();
    let init_allocated=Gc.allocated_bytes() in
    Gc.compact();
    let arbre =compresseABR arbre in
    (* Call GC to clean all garbages *)
    Gc.compact ();
    let now_allocated=Gc.allocated_bytes() in
    Printf.fprintf out "%d %f\n" i (now_allocated -. init_allocated)
  done;
  close_out out
