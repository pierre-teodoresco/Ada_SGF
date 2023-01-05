# Raffinage des sous-programmes du package P_Arbre

#### Ajouter
R0: Ajouter un noeud dans un arbre en paramètre
R1: Comment R0?
```
    Si F_noeud = NULL Alors
        F_noeud^.Fils <- F_fils
    Sinon
A11     Se placer sur le dernier fils de F_noeud        in F_noeud, out T_noeud
A12     Ajouter à la liste de ces fils F_fils           in out T_noeud, in F_fils
    Fin Si
A13 Mettre à jour le Père de F_fils                     out F_fils, in F_noeud
```
```
R2: Comment A11?
```
    T_noeud <- F_noeud^.Fils
    TantQue T_noeud^.Frere /= NULL Faire
        T_noeud <- T_noeud^.Frere
    Fin TantQue
```
R2: Comment A12?
```
    T_noeud^.Frere <- F_fils
```
R2: Comment A13?
```
    F_fils^.Pere <- F_noeud
```

#### Affichage

R0: Afficher un arbre
R1: Comment R0?
```
    Si F_noeud = NULL Alors
        Afficher "Arbre vide"
    Sinon
A11     Afficher le noeud F_noeud               in F_noeud
A12     Afficher les fils de F_noeud            in F_noeud
A13     Afficher les frères de F_noeud          in F_noeud
    Fin Si
```
R2: Comment A11?
```
    Ecrire(F_noeud^.Valeur)
```
R2: Comment A12?
```
    Afficher(F_noeud^.Fils)
```
R2: Comment A13?
```
    Afficher(F_noeud^.Frere)
```
