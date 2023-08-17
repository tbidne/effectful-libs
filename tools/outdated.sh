set -e

export LANG="C.UTF-8"

export dirs="lib/*-effectful"

for d in $dirs; do
  echo "$d"
  cd "$d"
  cabal outdated
  echo ""
  cd ../../
done