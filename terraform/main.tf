provider "google" {
    region = "us-central1"
    project = "eums-162320"
    credentials = "${file("/gcloud/credentials")}"
}

variable username {}
variable password {}

resource "google_container_cluster" "eums" {
    name = "eums"
    description = "eums"
    zone = "us-central1-a"
    initial_node_count = "3"

    master_auth {
        username = "${var.username}"
        password = "${var.password}"
    }

    node_config {
        machine_type = "n1-standard-1"

        oauth_scopes = [
            "https://www.googleapis.com/auth/compute",
            "https://www.googleapis.com/auth/devstorage.read_only",
            "https://www.googleapis.com/auth/logging.write",
            "https://www.googleapis.com/auth/monitoring",
        ]
    }
}
