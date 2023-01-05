# Raffinage des sous-programmes du package P_Arbre

#### Ajouter
R0: Ajouter un noeud dans un arbre en paramètres
R1: Comment R0?
```
    Si F_noeud = NULL Alors
        F_noeud^.Fils <- F_fils
    Sinon
A11 Se placer sur le dernier fils de F_noeud        in F_noeud, out T_noeud
A12 Ajouter à la liste de ces fils F_fils
    Fin Si
A13 Mettre à jour le Père de F_fils
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

