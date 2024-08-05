#!/bin/sh

# Exit immediately if a command exits with a non-zero status
set -e

# Define paths to required utilities
JQ="$(nix build -f '<nixpkgs>' jq.bin --print-out-paths --no-link)/bin/jq"

# Prefetch the current flake's store path
SELF="$(nix flake prefetch . --json | $JQ -r .storePath)"

TAG=${TAG:-latest}

# Copy the flake to the remote builder
nix copy $SELF --to ssh://nixremotebuilderamd64
nix copy $SELF --to ssh://nixremotebuilderarm64

# Build the Docker image on the remote machines
AMD64IMAGE="$(ssh nixremotebuilderamd64 nix build $SELF#container-default/container --no-link --print-out-paths)"
ARM64IMAGE="$(ssh nixremotebuilderarm64 nix build $SELF#container-default/container --no-link --print-out-paths)"

# Copy the resulting Docker image files back to the local machine
scp nixremotebuilderamd64:$AMD64IMAGE ./docker_image_amd64.tar
scp nixremotebuilderarm64:$ARM64IMAGE ./docker_image_arm64.tar

# Load Docker images
docker load < ./docker_image_amd64.tar
docker load < ./docker_image_arm64.tar

# Verify that images are loaded and get their IDs
AMD64_IMAGE_ID=$(docker images -q devnet:container-default | head -n 1)
ARM64_IMAGE_ID=$(docker images -q devnet:container-default | tail -n 1)

# Ensure the images are loaded correctly
if [ -z "$AMD64_IMAGE_ID" ] || [ -z "$ARM64_IMAGE_ID" ]; then
  echo "Error: Docker images were not loaded correctly."
  exit 1
fi

# Tag the Docker images with the correct tags
docker tag $AMD64_IMAGE_ID kadena/devnet:latest-amd64
docker tag $ARM64_IMAGE_ID kadena/devnet:latest-arm64

# Remove old manifest if it exists
docker manifest rm kadena/devnet:$TAG || true

# Create and push the Docker manifest
docker manifest create kadena/devnet:$TAG --amend kadena/devnet:latest-amd64 --amend kadena/devnet:latest-arm64
docker manifest push kadena/devnet:$TAG
