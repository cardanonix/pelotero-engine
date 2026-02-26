{ lib, pkgs, config ? { } }:

let
  defaultConfig = {
    projectRoot = ".";
    hsDirs = [ "src" "src-new" "app" ];

    hsConfig = {
      cabalFile = null;
      extensions = [ ".hs" ];
    };
    nixConfig = {
      extensions = [ ".nix" ];
      dirs = [ "." "nix" ];
    };

    excludePatterns = [
      ".spago"
      "node_modules"
      "dist"
      "dist-newstyle"
      "output"
    ];
  };

  cfg = lib.recursiveUpdate defaultConfig config;

  excludePatternStr = lib.concatMapStringsSep "\\|" (p: p) cfg.excludePatterns;

  generateManifestScript = pkgs.writeShellScriptBin "generate-manifest" ''
    set -euo pipefail

    PROJECT_ROOT="$(pwd)"
    SCRIPT_DIR="$PROJECT_ROOT/script"
    MANIFEST_FILE="$SCRIPT_DIR/manifest.json"
    mkdir -p "$SCRIPT_DIR"

    echo "Generating manifest..."

    # Find Haskell files
    echo "Finding Haskell files..."
    HS_FILES=()
    for dir in ${lib.concatStringsSep " " cfg.hsDirs}; do
      full_dir="$PROJECT_ROOT/$dir"
      if [ -d "$full_dir" ]; then
        echo "Scanning $full_dir for Haskell files"
        while IFS= read -r file; do
          if [ -n "$file" ]; then
            rel_path="''${file#$PROJECT_ROOT/}"
            HS_FILES+=("$rel_path")
          fi
        done < <(find "$full_dir" -type f -name "*.hs" 2>/dev/null | grep -v "${excludePatternStr}" | sort)
      fi
    done

    # Find Nix files
    echo "Finding Nix files..."
    NIX_FILES=()

    if [ -d "$PROJECT_ROOT" ]; then
      while IFS= read -r file; do
        rel_path="''${file#$PROJECT_ROOT/}"
        if [ -f "$file" ] && [[ "$file" != *"/script/concat_archive/"* ]]; then
          NIX_FILES+=("$rel_path")
        fi
      done < <(find "$PROJECT_ROOT" -maxdepth 1 -type f -name "*.nix" 2>/dev/null | sort)
    fi

    if [ -d "$PROJECT_ROOT/nix" ]; then
      while IFS= read -r file; do
        rel_path="''${file#$PROJECT_ROOT/}"
        NIX_FILES+=("$rel_path")
      done < <(find "$PROJECT_ROOT/nix" -type f -name "*.nix" 2>/dev/null | sort)
    fi

    # Write manifest JSON
    echo "{" > $MANIFEST_FILE
    echo "  \"meta\": {" >> $MANIFEST_FILE
    echo "    \"generated\": \"$(date '+%s')\"," >> $MANIFEST_FILE
    echo "    \"humanTime\": \"$(date '+%Y-%m-%d %H:%M:%S')\"," >> $MANIFEST_FILE
    echo "    \"projectRoot\": \"$PROJECT_ROOT\"" >> $MANIFEST_FILE
    echo "  }," >> $MANIFEST_FILE

    # Haskell section
    echo "  \"haskell\": {" >> $MANIFEST_FILE
    echo "    \"include\": [" >> $MANIFEST_FILE
    if [ ''${#HS_FILES[@]} -gt 0 ]; then
      for i in "''${!HS_FILES[@]}"; do
        if [ $i -eq $((''${#HS_FILES[@]} - 1)) ]; then
          echo "      \"''${HS_FILES[$i]}\"" >> $MANIFEST_FILE
        else
          echo "      \"''${HS_FILES[$i]}\"," >> $MANIFEST_FILE
        fi
      done
    fi
    echo "    ]," >> $MANIFEST_FILE
    echo "    \"exclude\": []," >> $MANIFEST_FILE
    echo "    \"count\": ''${#HS_FILES[@]}," >> $MANIFEST_FILE
    echo "    \"timestamp\": \"$(date '+%s')\"" >> $MANIFEST_FILE
    echo "  }," >> $MANIFEST_FILE

    # Nix section
    echo "  \"nix\": {" >> $MANIFEST_FILE
    echo "    \"include\": [" >> $MANIFEST_FILE
    if [ ''${#NIX_FILES[@]} -gt 0 ]; then
      for i in "''${!NIX_FILES[@]}"; do
        if [ $i -eq $((''${#NIX_FILES[@]} - 1)) ]; then
          echo "      \"''${NIX_FILES[$i]}\"" >> $MANIFEST_FILE
        else
          echo "      \"''${NIX_FILES[$i]}\"," >> $MANIFEST_FILE
        fi
      done
    fi
    echo "    ]," >> $MANIFEST_FILE
    echo "    \"exclude\": []," >> $MANIFEST_FILE
    echo "    \"count\": ''${#NIX_FILES[@]}," >> $MANIFEST_FILE
    echo "    \"timestamp\": \"$(date '+%s')\"" >> $MANIFEST_FILE
    echo "  }" >> $MANIFEST_FILE
    echo "}" >> $MANIFEST_FILE

    # Pretty-print
    ${pkgs.jq}/bin/jq . "$MANIFEST_FILE" > "$MANIFEST_FILE.tmp" && mv "$MANIFEST_FILE.tmp" "$MANIFEST_FILE"

    # Backup
    BACKUP_TIME=$(date '+%Y%m%d_%H%M%S')
    cp "$MANIFEST_FILE" "$MANIFEST_FILE.$BACKUP_TIME"

    echo "Manifest generated at: $MANIFEST_FILE"
    echo "Found ''${#HS_FILES[@]} Haskell files, ''${#NIX_FILES[@]} Nix files"
  '';

in {
  generateScript = generateManifestScript;

  debug = {
    config = cfg;
    excludePattern = excludePatternStr;
  };
}