# Raffinage des sous-programmes du package P_Arbre

#### Ajouter
##### signature: procedure Ajouter(F_noeud: in out Arbre; F_fils: in Arbre)
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
##### signature: procedure Affichage(F_arbre: in Arbre)
R0: Afficher un arbre
R1: Comment R0?
```
    Si F_arbre = NULL Alors
        Rien
    Sinon
A11     Afficher le noeud F_arbre               in F_arbre
A12     Afficher les fils de F_arbre            in F_arbre
A13     Afficher les frères de F_arbre          in F_arbre
    Fin Si
```
R2: Comment A11?
```
    Afficher_noeud(F_arbre)
```
R2: Comment A12?
```
    Afficher(F_arbre^.Fils)
```
R2: Comment A13?
```
    Afficher(F_arbre^.Frere)
```

#### Afficher_noeud
##### signature: procedure Afficher_noeud(F_noeud: in Arbre)
R0: Afficher un noeud
R1: Comment R0?
```
    T_fils <- F_noeud^.Fils
    T_frere <- F_noeud^.Frere
A11 Afficher la valeur de F_noeud           in F_noeud
A12 Afficher le père de F_noeud             in F_noeud
A13 Afficher les freres de F_noeud          in T_frere
A14 Afficher les fils de F_noeud            in T_fils
```
R2: Comment A11?
```
    Ecrire(F_noeud^.Valeur)
```
R2: Comment A12?
```
    Ecrire("Pere: ")
    Ecrire(F_noeud^.Pere^.Valeur)
```
R2: Comment A13?
```
    Ecrire("Frere: ")
    TantQue T_frere /= NULL Faire
        Ecrire(T_frere^.Valeur)
        T_frere <- T_frere^.Frere
    Fin TantQue
```
R2: Comment A14?
```
    Ecrire("Fils: ")
    TantQue T_fils /= NULL Faire
        Ecrire(T_fils^.Valeur)
        T_fils <- T_fils^.Frere
    Fin TantQue
```
#### Detruire
##### signature: procedure Detruire(F_noeud: in out Arbre)
R0: Détruire un arbre
R1: Comment R0
```
    Si F_noeud = NULL Alors
        Rien
    Sinon
A11     Détruire les fils de F_noeud            in F_noeud
A12     Détruire les frères de F_noeud          in F_noeud
A13     Détruire le noeud F_noeud               in F_noeud
    Fin Si
```
R2: Comment A11?
```
    Detruire(F_noeud^.Fils)
```
R2: Comment A12?
```
    Detruire(F_noeud^.Frere)
```
R2: Comment A13?
```
    Liberer(F_noeud)
```