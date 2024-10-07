#!/bin/bash

# Define the groups you want to ensure exist and add the user to
groups=("video" "dialout" "audio" "plugdev" "cdrom" "storage" "scanner" "optical" "kvm" "input" "floppy" "disk" "render")
user="st"

# Loop through the groups and create them if they don't exist
for group in "${groups[@]}"; do
    if ! getent group "$group" > /dev/null 2>&1; then
        sudo groupadd "$group"
        echo "Created group: $group"
    fi
done

# Add the user to the groups
sudo usermod -aG "$(IFS=,; echo "${groups[*]}")" "$user"

echo "Added user $user to groups: ${groups[*]}"
