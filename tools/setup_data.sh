#!/usr/bin/env bash
# Installe et configure DVC et DagsHub pour ce dépôt (Linux / macOS).
#
# Usage :
#   bash tools/setup_data.sh           # installation et configuration
#   bash tools/setup_data.sh --pull    # idem, puis téléchargement des données (dvc pull)
#
# Les identifiants sont demandés au clavier (token masqué). En mode non
# interactif, ils sont lus dans les variables DAGSHUB_USER et DAGSHUB_TOKEN.
set -euo pipefail

PULL=0
case "${1:-}" in
  "") ;;
  --pull) PULL=1 ;;
  *) echo "Usage : bash tools/setup_data.sh [--pull]" >&2; exit 2 ;;
esac

cd "$(dirname "${BASH_SOURCE[0]}")/.."

if ! grep -q 'remote "dagshub"' .dvc/config 2>/dev/null; then
  echo "Erreur : remote DVC 'dagshub' introuvable dans .dvc/config." >&2
  exit 1
fi
command -v python3 >/dev/null || { echo "Erreur : python3 est requis." >&2; exit 1; }

echo "==> Environnement virtuel (.venv) et dépendances"
if [ ! -x .venv/bin/python ]; then
  python3 -m venv .venv || { echo "Erreur : sur Debian/Ubuntu, installez le paquet python3-venv." >&2; exit 1; }
fi
.venv/bin/python -m pip install --quiet --upgrade pip
.venv/bin/python -m pip install --quiet -r requirements.txt

echo "==> Identifiants DagsHub"
DS_USER="${DAGSHUB_USER:-}"
DS_TOKEN="${DAGSHUB_TOKEN:-}"
if [ -z "$DS_USER" ]; then read -rp "Nom d'utilisateur DagsHub : " DS_USER; fi
if [ -z "$DS_TOKEN" ]; then read -rsp "Token DagsHub (saisie masquée) : " DS_TOKEN; echo; fi
if [ -z "$DS_USER" ] || [ -z "$DS_TOKEN" ]; then
  echo "Erreur : nom d'utilisateur et token sont obligatoires." >&2
  exit 1
fi

echo "==> Configuration de DVC (.dvc/config.local, ignoré par Git)"
umask 077
touch .dvc/config.local
chmod 600 .dvc/config.local
.venv/bin/dvc remote modify --local dagshub auth basic
.venv/bin/dvc remote modify --local dagshub user "$DS_USER"
.venv/bin/dvc remote modify --local dagshub password "$DS_TOKEN"

echo "==> Connexion du client DagsHub"
.venv/bin/dagshub login --token "$DS_TOKEN" --quiet

if [ "$PULL" -eq 1 ]; then
  echo "==> Téléchargement des données (dvc pull)"
  .venv/bin/dvc pull
fi

echo
echo "Configuration terminée."
echo "Activez l'environnement :  source .venv/bin/activate"
if [ "$PULL" -eq 0 ]; then echo "Récupérez les données :    dvc pull"; fi
