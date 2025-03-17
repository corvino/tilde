#! /usr/bin/env bash

#echo $@

while [[ $# -gt 0 ]]; do
  case $1 in
    --title)
      TITLE="$2"
      shift # past argument
      shift # past value
      ;;
    --TVSeasonNum)
      SEASON="$2"
      shift # past argument
      shift # past value
      ;;
    --TVEpisodeNum)
      EPISODE="$2"
      shift # past argument
      shift # past value
      ;;
    --TVShowName)
      SHOW="$2"
      shift # past argument
      shift # past value
      ;;
    --default)
      DEFAULT=YES
      shift # past argument
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

set -- "${POSITIONAL_ARGS[@]}"

if [ "$#" -ne 0 ] || \
   [ -z ${TITLE+x} ] || \
   [ -z ${SHOW+x} ] || \
   [ -z ${SEASON+x} ] || \
   [ -z ${EPISODE+x} ]
then
    echo "usage: media-metadata-update.sh <mp4-file> ..."
    exit
fi

FILE=`ls -1 -t ~/Downloads/* | head -n 1`

atomicparsley "${FILE}" --overWrite --stik "TV Show" --title "${TITLE}" --TVShowName "${SHOW}" --TVSeasonNum "${SEASON}" --TVEpisodeNum "${EPISODE}"
open -a TV.app "${FILE}"
sleep 3
rm "${FILE}"
