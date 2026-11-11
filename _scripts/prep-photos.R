# Prepare photos for a blog post: resize, strip metadata (incl. GPS) and save as WebP.
#
# Usage (from the project root):
#   Rscript _scripts/prep-photos.R <source> <img-dir> [--overwrite]
#
#   <source>      a .zip of photos (e.g. a Google Photos download) or a folder of images
#   <img-dir>     the post's image folder, e.g. blog/2026-11-11-walks/img
#   --overwrite   replace WebP files that already exist (skipped by default)
#
# Example:
#   Rscript _scripts/prep-photos.R ~/Downloads/Photos-1-001.zip blog/2026-11-11-walks/img
#
# Originals are never modified. Keep them in your backup, not in the repository.

library(magick)

max_side <- 1600   # longest edge in pixels
quality  <- 85     # WebP quality (0-100)

args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--overwrite" %in% args
args <- setdiff(args, "--overwrite")

if (length(args) != 2) {
  stop("Usage: Rscript _scripts/prep-photos.R <source zip or folder> <img-dir> [--overwrite]",
       call. = FALSE)
}

source_path <- normalizePath(args[1], mustWork = TRUE)
img_dir <- args[2]
dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)

# Extract a zip into its own empty temporary folder
if (grepl("[.]zip$", source_path, ignore.case = TRUE)) {
  src_dir <- tempfile("photos-")
  dir.create(src_dir)
  unzip(source_path, exdir = src_dir)
  on.exit(unlink(src_dir, recursive = TRUE), add = TRUE)
} else {
  src_dir <- source_path
}

photos <- list.files(src_dir, pattern = "[.](jpe?g|png|heic|webp)$",
                     ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

if (length(photos) == 0) stop("No images found in ", args[1], call. = FALSE)

n_done <- 0
n_skipped <- 0

for (f in photos) {
  out <- file.path(img_dir, paste0(tools::file_path_sans_ext(basename(f)), ".webp"))

  if (file.exists(out) && !overwrite) {
    n_skipped <- n_skipped + 1
    next
  }

  image_read(f) |>
    image_orient() |>                                     # apply phone rotation
    image_scale(paste0(max_side, "x", max_side, ">")) |>  # only ever shrink
    image_strip() |>                                      # remove EXIF and GPS
    image_write(out, format = "webp", quality = quality)

  n_done <- n_done + 1
}

cat(sprintf("Saved %d WebP image(s) to %s; skipped %d that already existed.\n",
            n_done, img_dir, n_skipped))
