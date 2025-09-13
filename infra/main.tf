locals {
  project_slug = "nixos"
  project_name = "NixOS"

  target_compartment = oci_identity_compartment.this.id
  vcn_cidr           = "10.0.0.0/16"
}

resource "oci_identity_compartment" "this" {
  name        = "${local.project_slug}"
  description = "Compartment for ${local.project_name} managed instances."

  compartment_id = var.tenancy_ocid
}

resource "oci_core_vcn" "this" {
  display_name   = "${local.project_slug}-vcn"
  cidr_block     = local.vcn_cidr
  is_ipv6enabled = true

  compartment_id = local.target_compartment
}

resource "oci_core_subnet" "public" {
  display_name      = "${local.project_slug}-vcn-public-1"
  cidr_block        = cidrsubnet(local.vcn_cidr, 8, 0)
  vcn_id            = oci_core_vcn.this.id
  route_table_id    = oci_core_route_table.public.id
  security_list_ids = null

  compartment_id = local.target_compartment
}

resource "oci_core_internet_gateway" "this" {
  display_name = "${local.project_slug}-vcn-igw"
  vcn_id       = oci_core_vcn.this.id
  enabled      = "true"

  compartment_id = local.target_compartment
}

resource "oci_core_route_table" "public" {
  display_name = "${local.project_slug}-vcn-public-1"
  route_rules {
    network_entity_id = oci_core_internet_gateway.this.id
    destination       = "0.0.0.0/0"
  }
  vcn_id = oci_core_vcn.this.id

  compartment_id = local.target_compartment
}

resource "oci_core_default_security_list" "lockdown" {
  manage_default_resource_id = oci_core_vcn.this.default_security_list_id

  lifecycle {
    ignore_changes = [egress_security_rules, ingress_security_rules, defined_tags]
  }

  compartment_id = local.target_compartment
}

resource "oci_core_network_security_group" "instance" {
  display_name = "${local.project_slug}-instance-nsg"
  vcn_id       = oci_core_vcn.this.id

  compartment_id = local.target_compartment
}

data "external" "public_ip" {
  program = [
    "sh",
    "-c",
    "curl -s ifconfig.co | xargs -I {} echo '{\"ip\":\"{}\"}'"
  ]
}

locals {
  public_ip      = data.external.public_ip.result.ip
  public_ip_cidr = "${local.public_ip}/32"
}

resource "oci_core_network_security_group_security_rule" "instance_ssh" {
  description               = "Allow SSH traffic"
  network_security_group_id = oci_core_network_security_group.instance.id
  direction                 = "INGRESS"
  protocol                  = "6" # TCP
  source_type               = "CIDR_BLOCK"
  source                    = "0.0.0.0/0"
  # source                    = local.public_ip_cidr

  tcp_options {
    destination_port_range {
      min = 22
      max = 22
    }
    source_port_range {
      min = 1
      max = 65535
    }
  }
}

resource "oci_core_network_security_group_security_rule" "instance_egress" {
  description               = "Allow all egress traffic"
  network_security_group_id = oci_core_network_security_group.instance.id
  direction                 = "EGRESS"
  protocol                  = "all"
  destination_type          = "CIDR_BLOCK"
  destination               = "0.0.0.0/0"
}

data "oci_identity_availability_domains" "current" {
  compartment_id = local.target_compartment
}

locals {
  ads = [for i in data.oci_identity_availability_domains.current.availability_domains : i.name]
}

data "oci_core_images" "ubuntu" {
  operating_system = "Canonical Ubuntu"
  shape            = "VM.Standard.A1.Flex"
  state            = "AVAILABLE"

  filter {
    name   = "operating_system_version"
    values = ["24.04 Minimal.*"]
    regex  = true
  }

  sort_by    = "TIMECREATED"
  sort_order = "DESC"

  compartment_id = local.target_compartment
}

resource "oci_core_instance" "this" {
  display_name = "apollo"

  shape = "VM.Standard.A1.Flex"
  shape_config {
    memory_in_gbs = "24"
    ocpus         = "4"
  }

  create_vnic_details {
    assign_ipv6ip             = "false"
    assign_private_dns_record = "true"
    assign_public_ip          = "true"
    subnet_id                 = oci_core_subnet.public.id
    nsg_ids = [oci_core_network_security_group.instance.id]
  }

  source_details {
    boot_volume_size_in_gbs = "200"
    source_id               = data.oci_core_images.ubuntu.images[0].id
    source_type             = "image"
  }

  metadata = {
    "ssh_authorized_keys" = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBR9GjKkCrbAbfuQJXuMTh1I6agrhiHrxlEXhWgidvLS"
  }

  compartment_id      = local.target_compartment
  availability_domain = local.ads[0]
}

module "deploy" {
  source                 = "github.com/nix-community/nixos-anywhere/terraform/all-in-one"

  nixos_system_attr      = "..#nixosConfigurations.apollo.config.system.build.toplevel"
  nixos_partitioner_attr = "..#nixosConfigurations.apollo.config.system.build.diskoScript"
  target_host            = oci_core_instance.this.public_ip
  target_user            = "mario"
  instance_id            = oci_core_instance.this.id
  install_user           = "ubuntu"
}
