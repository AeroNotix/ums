provider "google" {
  region = var.region
  project = var.project
  zone = var.zone
}

resource "google_container_cluster" "eums" {
  name = var.cluster-name
  description = var.cluster-description

  initial_node_count = "3"

  node_config {
    machine_type = "g1-small"

    oauth_scopes = [
      "https://www.googleapis.com/auth/compute",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
    ]
  }
}
