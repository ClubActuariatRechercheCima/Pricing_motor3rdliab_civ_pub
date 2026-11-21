# Pricing RC Automobile – Revue du tarif réglementaire en Côte d'Ivoire

> Projet de recherche du **Club Actuariat Recherche CIMA** : évaluer, près de vingt ans après son introduction, la pertinence du tarif réglementaire minimum de l'assurance responsabilité civile automobile, et proposer de nouveaux critères de tarification.

![Statut](https://img.shields.io/badge/statut-en%20cours-blue)
![Langage](https://img.shields.io/badge/langage-R-276DC3)
![Données](https://img.shields.io/badge/donn%C3%A9es-DVC%20%2B%20DagsHub-945DD6)

---

## Sommaire

1. [Contexte et objectifs](#contexte-et-objectifs)
2. [Méthodologie](#méthodologie)
3. [Aperçu des résultats](#aperçu-des-résultats)
4. [Structure du dépôt](#structure-du-dépôt)
5. [Accès aux données (DagsHub + DVC)](#accès-aux-données-dagshub--dvc)
6. [Installation et reproduction](#installation-et-reproduction)
7. [Documentation et article](#documentation-et-article)
8. [Contribuer](#contribuer)

---

## Contexte et objectifs

En Côte d'Ivoire, l'assurance responsabilité civile (RC) des véhicules terrestres à moteur est obligatoire. Dans le cadre du traité CIMA (1996), un **tarif minimum réglementaire** a été fixé par arrêté ministériel en **2003**. Il définit les critères et les modalités de calcul de la prime que tout propriétaire de véhicule doit souscrire.

L'assurance automobile est la première branche non vie du marché ivoirien (32 % des primes émises en 2021), et la RC en représente environ la moitié. Après quasiment 20 ans d'application, ce projet cherche à :

- **évaluer la pertinence** des critères du tarif réglementaire, notamment leur pouvoir discriminant ;
- **mesurer la capacité de la prime** à couvrir les sinistres au regard de l'inflation des coûts ;
- **proposer de nouveaux critères** de tarification ;
- **promouvoir l'usage des techniques actuarielles** dans les travaux de tarification.

C'est le premier d'une série de projets sur des thématiques actuarielles variées : l'organisation du dépôt sert de cadre commun aux projets suivants.

## Méthodologie

1. **Préparation des données** : traitement des valeurs aberrantes et manquantes, regroupement de modalités, traitement des sinistres extrêmes, détection d'anomalies (doublons, charges ou paiements négatifs, lignes sans flux).
2. **Analyse descriptive** : structure du portefeuille et sinistralité par usage du véhicule, zone géographique, type de moteur et puissance fiscale.
3. **Diagnostic statistique du tarif** : modèles linéaires généralisés (**GLM**) et **arbres de régression** pour mesurer le pouvoir discriminant des critères réglementaires.
4. **Nouvelles variables tarifaires** : ajout de variables supplémentaires puis comparaison des modèles.
5. **Résultats et recommandations**.

Les analyses sont réalisées sous **R**.

## Aperçu des résultats

Quelques-uns des graphiques produits (dossier [`Results/`](Results/)) :

| Primes émises en automobile (Côte d'Ivoire) | Distribution de la charge de sinistres |
|:---:|:---:|
| ![Primes émises](Results/CA_Marche.png) | ![Distribution de la charge](Results/distribution_CHARGE_SINIETRE.png) |

| Ratio S/P automobile – Corée | Ratio S/P automobile – France |
|:---:|:---:|
| ![S/P Corée](Results/SP_Auto_Coree.png) | ![S/P France](Results/SP_Auto_France.png) |

| Ratios S/P RC et Dommage – Côte d'Ivoire | Distribution de la charge nette |
|:---:|:---:|
| ![S/P Côte d'Ivoire](Results/SP_CI_Auto_Dommage.png) | ![Charge nette](Results/distribution_CHARE_NETTE.png) |

Un tableau de synthèse des anomalies de données détectées est disponible dans [`Results/Anomalies/ResumeAnomalie.csv`](Results/Anomalies/ResumeAnomalie.csv).

## Structure du dépôt

```text
.
├── Data.dvc          # Pointeur DVC vers les données (stockées sur DagsHub)
├── Docs/             # Documentation : comptes rendus, agenda, formation Git & DVC
├── Env/              # Environnement de travail (versions des logiciels et librairies)
├── Notebooks/        # Notebooks (Python, R, etc.)
├── Results/          # Graphiques, tableaux et article
│   ├── Anomalies/    #   synthèse des anomalies de données
│   └── Articles/     #   version de travail de l'article
└── Src/              # Scripts R ayant produit les analyses
```

Le dossier `Data/` n'est **pas versionné par Git** : il est géré par DVC (voir ci-dessous).

## Accès aux données (DagsHub + DVC)

Les données ne sont pas stockées dans Git. Elles sont suivies par [DVC](https://dvc.org) et hébergées sur DagsHub :

| | |
|---|---|
| **Dépôt DagsHub** | <https://dagshub.com/ClubActuariatRechercheCima/Pricing_motor3rdliab_civ_pub> |
| **Remote DVC** | `https://dagshub.com/ClubActuariatRechercheCima/Pricing_motor3rdliab_civ_pub.dvc` |
| **Branche** | `publication` |

Le fichier [`Data.dvc`](Data.dvc) contient l'empreinte (hash) du jeu de données de la version courante ; DVC s'en sert pour récupérer les bons fichiers.

### 1. Prérequis

- [Git](https://git-scm.com) et [DVC](https://dvc.org/doc/install) (`pip install dvc`) ;
- un compte [DagsHub](https://dagshub.com) disposant des droits de lecture sur le dépôt.

### 2. Récupérer le projet et les données

```bash
git clone https://github.com/ClubActuariatRechercheCima/Pricing_motor3rdliab_civ_pub.git
cd Pricing_motor3rdliab_civ_pub
git checkout publication

# Authentification DagsHub (à stocker en local, jamais dans Git)
dvc remote modify --local dagshub auth basic
dvc remote modify --local dagshub user <votre_nom_utilisateur_dagshub>
dvc remote modify --local dagshub password <votre_token_dagshub>

# Téléchargement des données dans Data/
dvc pull
```

Le token d'accès se génère dans DagsHub, sous *Paramètres* → *Tokens*. L'option `--local` l'enregistre dans `.dvc/config.local`, un fichier ignoré par Git.

### 3. Vérifier que tout est synchronisé

```bash
dvc status -c      # doit afficher : Cache and remote 'dagshub' are in sync.
```

### 4. Contribuer des données

```bash
dvc add Data                 # met à jour Data.dvc
git add Data.dvc
git commit -m "Mise à jour des données"
dvc push                     # envoie les données sur DagsHub
git push
```

### Accès depuis Python

```python
import dagshub
dagshub.init(repo_owner="ClubActuariatRechercheCima",
             repo_name="Pricing_motor3rdliab_civ_pub", mlflow=False)
```

> **Important :** ne jamais ajouter de clés d'accès ou de mots de passe dans `.dvc/config` (versionné). Utiliser toujours `--local`.

## Installation et reproduction

1. Récupérer le code et les données (voir la section précédente).
2. Consulter les versions utilisées dans [`Env/`](Env/) :
   - `Requirements.txt` : librairies et leurs versions ;
   - `Software_version.txt` : versions des logiciels.
3. Exécuter les scripts R du dossier [`Src/`](Src/) (les plus anciens sont dans `Src/Script old/`).

## Documentation et article

- **Article** : *Revue du tarif réglementaire en assurance responsabilité civile automobile en Côte d'Ivoire*, Yoboua Angoua. Version de travail du 25 mai 2022 : [`Results/Articles/YA-Version du 25 Mai.pdf`](Results/Articles/YA-Version%20du%2025%20Mai.pdf).
- **Comptes rendus de réunion** : [`Docs/Compte_rendus/`](Docs/Compte_rendus/).
- **Agenda de finalisation des travaux** : [`Docs/Agenda_finalisation_travaux_article.xlsx`](Docs/Agenda_finalisation_travaux_article.xlsx).
- **Formation Git & DVC** : [`Docs/Formation_Git&Dvc/`](Docs/Formation_Git&Dvc/), avec les aide-mémoire Git et DVC.

## Contribuer

1. Créer une branche à partir de `publication`.
2. Commiter de petites modifications, avec des messages clairs.
3. Ne jamais versionner de données brutes dans Git : utiliser DVC.
4. Ouvrir une *pull request* pour relecture.

---

*Projet porté par le Club Actuariat Recherche CIMA.*
