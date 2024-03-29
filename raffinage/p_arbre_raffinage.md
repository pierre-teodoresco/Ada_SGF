# Raffinage des sous-programmes du package P_Arbre

#### Ajouter
##### signature: procedure Ajouter(F_noeud: in out Arbre; F_element: in Type_Element)
R0: Ajouter un noeud dans un arbre en paramètre
R1: Comment R0?
```
    Si F_noeud = NULL Alors
        F_noeud^.Fils <- new Noeud(Pere => F_noeud, Frere => null, Fils => null, Contenu => T_element)
    Sinon
A11     Se placer sur le dernier fils de F_noeud        in F_noeud, out T_noeud
A12     Ajouter à la liste de ces fils F_fils           in out T_noeud, in F_fils, in F_noeud
    Fin Si
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
    T_noeud^.Frere <- new Noeud(Pere => F_noeud, Frere => null, Fils => null, Contenu => T_element)
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

#### Supprimer
##### signature: procedure Supprimer(F_noeud: in out Arbre)
R0: Supprimer un noeud
R1: Comment R0?
```
    Si F_noeud = NULL Alors
        Rien
    Sinon
A11     Verifier si F_noeud est un "fils direct" (pas un frère) ou non      in F_noeud
A13     Supprimer les fils de F_noeud                                       in F_noeud
A14     Supprimer F_noeud                                                   in F_noeud
    Fin Si
```

R2: Comment A11?
```
    Si F_noeud^.Pere^.Fils = F_noeud Alors
        F_noeud^.Pere^.Fils <- F_noeud^.Frere
    Sinon
        T_noeud <- F_noeud^.Pere^.Fils
        TantQue T_noeud^.Frere /= F_noeud Faire
            T_noeud <- T_noeud^.Frere
        Fin TantQue
        T_noeud^.Frere <- F_noeud^.Frere
    Fin Si
```

R2: Comment A13?
```
    Detruire(F_noeud^.Fils)
```

R2: Comment A14?
```
    Liberer(F_noeud)
```

#### Deplacer
##### signature: procedure Deplacer(F_noeud: in out Arbre, F_nouveau_pere: in out Arbre)
R0: Déplacer un noeud
R1: Comment R0?
```
A11 Si F_noeud est un fils direct Alors
A12     Modifier le fils du père                in F_noeud
    Sinon
A13     Modifier les frères                     in F_noeud
    FinSi
A14 Retirer le frere de F_noeud  
A15
A16 Modifier le pere de F_noeud     in out F_noeud, in F_nouveau_pere
```

R2: Comment A11?
```
F_noeud^.Pere.Fils = F_noeud Alors
```

R2: Comment A12?
```
    F_noeud^.Pere^.Fils <- F_noeud^.Frere
```

R2: Comment A13?
```
    Frere_prec <- F_noeud^.Pere^.Fils
A21 Trouver le Frere précédents             in out Frere_prec, in F_noeud
A22 Sortir F_noeud de la liste des Frères   in out Frere_prec, in F_noeud
```

R3: Comment A21?
```
TantQue Frere_prec^.Frere /= F_noeud Faire
    Frere_prec <- Frere_prec^.Frere
FinTQ
```

R3: Comment A22?
```
Frere_prec^.Frere <- F_noeud^.Frere
```

R2: Comment A15?
```
F_noeud^.Frere <- null
```

R2: Comment A15?
```
    T_noeud <- F_nouveau_pere^.Fils
A23 Si F_nouveau_pere à un fils                     in F_nouveau_pere
        TantQue T_noeud^.Frere /= NULL Faire
            T_noeud <- T_noeud^.Frere
        FinTQ
        T_noeud^.Frere <- F_noeud
    Sinon
        F_nouveau_pere^.Fils <- F_noeud
    Fin Si
```

R3: Comment A23?
``` 
F_nouveau_pere^.Fils = null
```

R2: Comment A16?
```
F_noeud^.Pere <- F_nouveau_pere
```

#### Rechercher

##### signature: fonction Rechercher_fils(F_pere: in Arbre, F_element: in Type_Element) renvoie Arbre
R0: Rechercher un noeud, retourne le noeud trouvé ou null sinon
R1: Comment R0?
```
    Si Est_vide(F_pere) then
        Retourner NULL
    Sinon
A11     Chercher si F_pere à pour fils F_element
    FinSi
```

R2: Comment A11?
    T_noeud <- F_pere^.Fils
    TantQue T_noeud /= null loop
        Trouver un fils de F_pere qui a pour valeur F_element
    Fin TantQue
```

R3: Comment A21?
```
    Si Egal(T_noeud^.Contenu, F_element) then            Egal est un predicat generique dépendant du type Type_Element
        Renvoyer T_noeud
    Sinon
        T_noeud <- T_noeud^.Frere
    end if;
```
