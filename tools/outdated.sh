set -e

export LANG="C.UTF-8"

export dirs="lib/effectful-utils lib/*-effectful"

for d in $dirs; do
  echo "$d"
  cd "$d"
  out=$(cabal outdated)
  echo $out
  echo ""
  cd ../../

  if [[ $out != "All dependencies are up to date." ]]; then
    any_outdated=1
  fi
done

exit $any_outdated
