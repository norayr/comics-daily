VERSION="0.7.1"
set -x
git tag -d $VERSION
git push maemo :refs/tags/$VERSION
git tag $VERSION
git push maemo $VERSION

