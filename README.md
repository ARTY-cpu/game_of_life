# Le jeu de la vie

Le jeu de la vie de Conway écrit dans le langage OCAML, avec la librairie graphique.

Ce projet a été lourdement inspiré d'un TP trouvé sur Internet qui donnait la marche à suivre pour le nom, et découpage des fonctions.

## Explications

Le jeu de la vie est de la catégorie des automates cellulaires. Celle-ci permet de voir un organisme confiné ici pour les étudier et de voir l'ordre ou le chaos régner selon 2 règles simples ici.

- si une cellule a exactement trois voisines vivantes, elle est vivante à l’étape suivante.
- si une cellule a exactement deux voisines vivantes, elle reste dans son état actuel à l’étape suivante.
- pour tout autre cas, elle meurt à l'étape suivante.

## Compilation et démarrer le projet

Pour changer les paramètres, veuillez les modifier dans la fonction de point d'entrée :

```ocaml
let () = new_game 300 110 1000
```

Le premier paramètre est la taille du tableau. \
Le deuxième paramètre est le nombre de cellules placés aléatoirement sur le tableau. \
Le troisième paramètre est le nombre de générations pour lequel le programme va tourner.

### Compilation

Pour compiler le projet, veuillez d'abord installer Dune, et Graphics.

```bash
eval $(opam env)
opam install dune graphics
```

Pour compiler avec Dune et exécuter, se placer dans le dossier principal du projet et exécuter les commandes :

```bash
dune build
dune exec ./life.exe
```

