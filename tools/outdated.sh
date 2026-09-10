set -e

export LANG="C.UTF-8"

export dirs="lib/effectful-utils lib/*-effectful"

any_outdated=0
for d in $dirs; do
  cd "$d"
  # err to /dev/null to trim extraneous logs
  out=$(cabal outdated 2> /dev/null)
  cd ../../

  if [[ $out != "All dependencies are up to date." ]]; then
    any_outdated=1
    echo -e "\n*** $d ***"
    echo -e "$out\n"
  else
    echo "*** $d ***"
  fi
done

exit $any_outdated
