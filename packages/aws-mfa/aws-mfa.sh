#!/usr/bin/env bash
#
# Version: 1.1.0
#
set -ueo pipefail

DEBUG="${DEBUG:-0}"
if [ "${DEBUG}" -eq 1 ]; then
    set -x
fi


#DEPS
_aws="$(command -v aws)"
_cat="$(command -v cat)"
_dirname="$(command -v dirname)"
_grep="$(command -v grep)"
_jq="$(command -v jq)"

#CONST
PROFILE_NAME=mfa


_throw() {
    err="${1}"

    {
        case "${err}" in
            1)
                printf "No MFA devices\n"
                ;;
            2)
                printf "MFA authentication failed\n"
                ;;
            3)
                printf "Device number MUST be a non-negative integer number\n"
                ;;
            4)
                printf "Session duration number MUST be an integer number between 15 and 2160 (minutes)\n"
                ;;
        esac
    } >>/dev/stderr
    exit "${err}"
}


get_mfa_devices() {
    if ! devices="$("$_aws" iam list-mfa-devices | "$_jq" -er '.MFADevices[] | .SerialNumber | select(contains(":mfa/"))')"; then
        _throw 1
    fi
    printf "%s" "${devices}"
}


select_device() {
    devices_str="${1}"  # Possibly multi-line
    device_num="${2}"
    #
    # Parses `device_str` to an array, then select the desired element.
    # If `device_num` is provided and found in the array, the corresponding
    # element is returned. `device_num` will be ignored when there is only
    # one device in the array. If there are multiple devices and `device_num`
    # is not provided, user will be prompted for input.
    #
    IFS=$'\n' read -r -d '' -a devices <<<"${devices_str}" || true

    i=0
    if (( ${#devices[@]} > 1 )); then
        if (( ${#device_num} > 0 )) && (( device_num < ${#devices[@]} )); then
            (( i = device_num ))
        else
            {
                printf "Available MFA devices:\n"
                for ((j=0; j<"${#devices[@]}"; j++)); do
                    printf "\t%d: %s\n" "$j" "${devices[${j}]}"
                done
            } >>/dev/stderr
            read -rp 'Select desired device: ' i
        fi
    fi
    printf "%s" "${devices[${i}]}"
}


input_token_code() {
    device="${1}"
    read -rp "Insert token code for ${device}: " code
    printf "%s" "${code}"
}


get_temp_credentals() {
    mfa_serial="$1"
    mfa_code="$2"

    if ! temp_credentials="$("$_aws" sts get-session-token \
        --duration-seconds "${session_duration}" \
        --serial-number "${mfa_serial}" \
        --token-code "${mfa_code}" \
            | "$_jq" -er '.Credentials')"; then
        _throw 2
    fi

    printf "%s" "${temp_credentials}"
}


set_profile() {
    credentials="${1}"

    "$_aws" configure set aws_access_key_id "$("$_jq" -er '.AccessKeyId' <<<"${credentials}")" --profile "${PROFILE_NAME}"
    "$_aws" configure set aws_secret_access_key "$("$_jq" -er '.SecretAccessKey' <<<"${credentials}")" --profile "${PROFILE_NAME}"
    "$_aws" configure set aws_session_token "$("$_jq" -er '.SessionToken' <<<"${credentials}")" --profile "${PROFILE_NAME}"
    "$_aws" configure set expiration "$("$_jq" -er '.Expiration' <<<"${credentials}")" --profile "${PROFILE_NAME}"
}


main() {
    device_number="${1}"
    mfa_code="${2:-}"

    caller_id="$("$_aws" sts get-caller-identity | "$_jq" -r '.Arn')"
    printf "Current user: %s\n" "${caller_id}"

    mfa_devices="$(get_mfa_devices)"
    mfa_device="$(select_device "${mfa_devices}" "${device_number}")"

    if [ "${mfa_code}" = "" ]; then
        mfa_code="$(input_token_code "${mfa_device}")"
    fi

    temp_credentials="$(get_temp_credentals "${mfa_device}" "${mfa_code}")"
    if [ "${DEBUG}" -eq 1 ]; then
        printf "Temp Credentials: %s\n" "$("$_jq" -S <<<"${temp_credentials}")"
    fi

    set_profile "${temp_credentials}"

    printf "MFA authentication completed\n"
}


device_number=
session_duration=360  # minutes
while getopts ':d:m:' opt; do
    case "${opt}" in
        d)
            device_number="${OPTARG}"
            if ! printf "%s" "${device_number}" | "$_grep" -qE '^[0-9]+$'; then
                _throw 3
            fi
            ;;
        m)
            session_duration="${OPTARG}"
            if ! printf "%s" "${session_duration}" | "$_grep" -qE '^[0-9]+$'; then
                _throw 4
            fi
            if (( session_duration < 15 || session_duration > 2160 )); then
                _throw 4
            fi
            ;;
        :)
            echo "Option -${OPTARG} requires an argument."
            exit 99
            ;;
        *)
            echo "Invalid option: -${OPTARG}."
            exit 99
            ;;
    esac
done
shift $((OPTIND-1))

(( session_duration = session_duration * 60 ))  # seconds

main "${device_number}" "$@"
