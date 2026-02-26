{ pkgs, name, lib, hsDirs, hsConfig }:

let

  compile-manifest = pkgs.writeShellScriptBin "compile-manifest" ''
    set -euo pipefail

    HS_DIRS="${lib.concatStringsSep " " hsDirs}"

    PROJECT_ROOT="$(pwd)"
    SCRIPT_DIR="$PROJECT_ROOT/script"
    MANIFEST_FILE="$SCRIPT_DIR/manifest.json"
    BASE_DIR="$SCRIPT_DIR/concat_archive"
    HASH_DIR="$BASE_DIR/.hashes"
    OUTPUT_DIR="$BASE_DIR/output"
    ARCHIVE_DIR="$BASE_DIR/archive"
    mkdir -p "$OUTPUT_DIR" "$ARCHIVE_DIR" "$HASH_DIR"

    TIMESTAMP=$(date '+%Y%m%d_%H%M%S')

    calculate_hash() {
        local file_list="$1"
        if [ -z "$file_list" ]; then
            echo "empty"
            return
        fi
        echo "$file_list" | xargs sha256sum 2>/dev/null | sha256sum | cut -d' ' -f1
    }

    get_previous_hash() {
        local file_type=$1
        local hash_file="$HASH_DIR/''${file_type}_last_hash"
        if [ -f "$hash_file" ]; then
            cat "$hash_file"
        else
            echo ""
        fi
    }

    save_current_hash() {
        local file_type=$1
        local current_hash=$2
        echo "$current_hash" > "$HASH_DIR/''${file_type}_last_hash"
    }

    compile_haskell() {
        local project_dir=$1
        local temp_file=$(mktemp)

        (cd "$project_dir" && cabal build) > "$temp_file" 2>&1
        local build_status=$?

        echo "{-"
        if [ -s "$temp_file" ]; then
            if [ $build_status -eq 0 ]; then
                echo "COMPILE_STATUS: true"
                echo "BUILD_OUTPUT:"
                cat "$temp_file"
            else
                echo "COMPILE_STATUS: false"
                echo "BUILD_OUTPUT:"
                cat "$temp_file"
            fi
        else
            echo "COMPILE_STATUS: error"
            echo "BUILD_OUTPUT:"
            echo "No build output captured"
        fi
        echo "-}"
        rm "$temp_file"
    }

    clean_haskell() {
        perl -0777 -pe '
            my @pragmas;
            while ($_ =~ /(\{-# .+? #-\})/g) {
                push @pragmas, $1;
            }
            s/\{-# .+? #-\}\n?//g;
            s/(\s*)--.*$/$1/gm;
            s/\{-(?!.*COMPILE).*?-\}//gs;
            if (@pragmas) {
                my %seen;
                @pragmas = grep { !$seen{$_}++ } @pragmas;
                my $pragma_text = join("\n", @pragmas);
                $_ = "$pragma_text\n\n$_";
            }
        ' | cat -s | sed 's/[[:space:]]*$//'
    }

    clean_nix() {
        sed 's/\([ ]*\)#.*$/\1/' | \
        perl -0777 -pe 's!/\*[^*]*\*+(?:[^/*][^*]*\*+)*/!!gs' | \
        cat -s | \
        sed 's/[[:space:]]*$//'
    }

    get_relative_path() {
        local full_path=$1
        echo "''${full_path#$PROJECT_ROOT/}"
    }

    safe_archive() {
        local file_type=$1
        local ext
        case "$file_type" in
            hs) ext="hs" ;;
            nix) ext="nix" ;;
            *) ext="txt" ;;
        esac

        for existing in "$OUTPUT_DIR"/*."$ext"; do
            if [ -f "$existing" ]; then
                mv "$existing" "$ARCHIVE_DIR/"
            fi
        done
    }

    get_status_for_filename() {
        local content="$1"
        if echo "$content" | grep -q "COMPILE_STATUS: true"; then
            echo "OK"
        elif echo "$content" | grep -q "COMPILE_STATUS: false"; then
            echo "FAIL"
        else
            echo "UNKNOWN"
        fi
    }

    concatenate_files() {
        local file_type=$1
        local output_base=$2
        local clean_function=$3
        local comment_char=$4
        local compile_function=''${5:-""}

        if [ ! -f "$MANIFEST_FILE" ]; then
            echo "No manifest file found. Run generate-manifest first."
            return 1
        fi

        local file_list
        file_list=$(${pkgs.jq}/bin/jq -r ".''${file_type} // .haskell | .include[]?" "$MANIFEST_FILE" 2>/dev/null | while read -r f; do
            echo "$PROJECT_ROOT/$f"
        done)

        if [ -z "$file_list" ]; then
            echo "No $file_type files found in manifest"
            return 0
        fi

        local current_hash
        current_hash=$(calculate_hash "$file_list")
        local previous_hash
        previous_hash=$(get_previous_hash "$file_type")

        local file_count
        file_count=$(echo "$file_list" | wc -l)

        local compile_output=""
        if [ -n "$compile_function" ]; then
            compile_output=$($compile_function "$PROJECT_ROOT")
        fi

        local temp_file
        temp_file=$(mktemp)

        {
            echo "''${comment_char}-"
            echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')"
            echo "Hash: $current_hash"
            echo "Files from manifest: $file_count"
            echo "''${comment_char}-"
            echo ""

            if [ -n "$compile_output" ]; then
                echo "$compile_output"
                echo ""
            fi

            echo "$file_list" | while read -r file; do
                if [ -f "$file" ]; then
                    echo "$comment_char FILE: $(get_relative_path "$file")"
                    cat "$file" | eval "$clean_function"
                    echo "$comment_char END OF: $(get_relative_path "$file")"
                    echo ""
                else
                    echo "$comment_char WARNING: File not found: $(get_relative_path "$file")"
                    echo ""
                fi
            done
        } > "$temp_file"

        local status
        status=$(get_status_for_filename "$(cat "$temp_file")")
        local output_file="''${output_base}''${status}.$file_type"
        mv "$temp_file" "$output_file"

        save_current_hash "$file_type" "$current_hash"
        echo "Generated new $file_type file: $output_file"
    }

    if [ ! -f "$MANIFEST_FILE" ]; then
        echo "Manifest file not found at $MANIFEST_FILE"
        echo "Run 'generate-manifest' to create it first."
        exit 1
    fi

    safe_archive "hs"
    safe_archive "nix"

    hs_base="''${OUTPUT_DIR}/Haskell_''${TIMESTAMP}_"
    nix_base="''${OUTPUT_DIR}/Nix_''${TIMESTAMP}_"

    echo -e "\nProcessing files according to manifest..."

    concatenate_files "haskell" "$hs_base" "clean_haskell" "--" "compile_haskell"
    concatenate_files "nix" "$nix_base" "clean_nix" "#" ""

    echo "Concatenation complete. Output files are in $OUTPUT_DIR"
  '';

  compile-archive = pkgs.writeShellScriptBin "compile-archive" ''
    set -euo pipefail

    HS_DIRS="${lib.concatStringsSep " " hsDirs}"

    PROJECT_ROOT="$(pwd)"
    SCRIPT_DIR="$PROJECT_ROOT/script"
    BASE_DIR="$SCRIPT_DIR/concat_archive"
    OUTPUT_DIR="$BASE_DIR/output"
    mkdir -p "$OUTPUT_DIR"

    TIMESTAMP=$(date '+%Y%m%d_%H%M%S')

    echo "Creating full project archive (IGNORING manifest)..."

    clean_haskell() {
        perl -0777 -pe '
            my @pragmas;
            while ($_ =~ /(\{-# .+? #-\})/g) {
                push @pragmas, $1;
            }
            s/\{-# .+? #-\}\n?//g;
            s/(\s*)--.*$/$1/gm;
            s/\{-(?!.*COMPILE).*?-\}//gs;
            if (@pragmas) {
                my %seen;
                @pragmas = grep { !$seen{$_}++ } @pragmas;
                my $pragma_text = join("\n", @pragmas);
                $_ = "$pragma_text\n\n$_";
            }
        ' | cat -s | sed 's/[[:space:]]*$//'
    }

    clean_nix() {
        sed 's/\([ ]*\)#.*$/\1/' | \
        perl -0777 -pe 's!/\*[^*]*\*+(?:[^/*][^*]*\*+)*/!!gs' | \
        cat -s | sed 's/[[:space:]]*$//'
    }

    get_relative_path() {
        local full_path=$1
        echo "''${full_path#$PROJECT_ROOT/}"
    }

    # Scan ALL Haskell files
    echo "Scanning ALL Haskell files..."
    hs_files=""
    for dir in $HS_DIRS; do
      full_dir="$PROJECT_ROOT/$dir"
      if [ -d "$full_dir" ]; then
        while IFS= read -r -d "" f; do
          hs_files="$hs_files $f"
        done < <(find "$full_dir" -name "*.hs" -type f -print0 | sort -z)
      fi
    done

    # Scan ALL Nix files
    echo "Scanning ALL Nix files..."
    nix_files=""
    while IFS= read -r -d "" f; do
      nix_files="$nix_files $f"
    done < <(find "$PROJECT_ROOT" -maxdepth 1 -name "*.nix" -type f -print0 | sort -z)
    if [ -d "$PROJECT_ROOT/nix" ]; then
      while IFS= read -r -d "" f; do
        nix_files="$nix_files $f"
      done < <(find "$PROJECT_ROOT/nix" -name "*.nix" -type f -print0 | sort -z)
    fi

    # Haskell archive
    hs_output="$OUTPUT_DIR/Haskell_ARCHIVE_$TIMESTAMP.hs"
    {
        echo "{-"
        echo "FULL PROJECT ARCHIVE - Generated: $(date '+%Y-%m-%d %H:%M:%S')"
        echo "Contains ALL Haskell files (manifest IGNORED)"
        echo "Files: $(echo $hs_files | wc -w)"
        echo "-}"
        echo ""

        # Compile status
        temp_build=$(mktemp)
        (cd "$PROJECT_ROOT" && cabal build) > "$temp_build" 2>&1
        build_status=$?
        echo "{-"
        if [ $build_status -eq 0 ]; then
            echo "COMPILE_STATUS: true"
            echo "BUILD_OUTPUT:"
            cat "$temp_build"
        else
            echo "COMPILE_STATUS: false"
            echo "BUILD_OUTPUT:"
            cat "$temp_build"
        fi
        echo "-}"
        echo ""
        rm "$temp_build"

        for file in $hs_files; do
            if [ -f "$file" ]; then
                echo "-- FILE: $(get_relative_path "$file")"
                cat "$file" | clean_haskell
                echo "-- END OF: $(get_relative_path "$file")"
                echo ""
            fi
        done
    } > "$hs_output"
    echo "Created: $hs_output"

    # Nix archive
    nix_output="$OUTPUT_DIR/Nix_ARCHIVE_$TIMESTAMP.nix"
    {
        echo "# FULL PROJECT ARCHIVE - Generated: $(date '+%Y-%m-%d %H:%M:%S')"
        echo "# Contains ALL Nix files (manifest IGNORED)"
        echo "# Files: $(echo $nix_files | wc -w)"
        echo ""
        for file in $nix_files; do
            if [ -f "$file" ]; then
                echo "# FILE: $(get_relative_path "$file")"
                cat "$file" | clean_nix
                echo "# END OF: $(get_relative_path "$file")"
                echo ""
            fi
        done
    } > "$nix_output"
    echo "Created: $nix_output"

    echo ""
    echo "Full archive complete. Files in $OUTPUT_DIR"
    echo "  Haskell: $(echo $hs_files | wc -w) files"
    echo "  Nix: $(echo $nix_files | wc -w) files"
  '';

in {
  inherit compile-manifest compile-archive;
}