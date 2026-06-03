{
  pkgs,
  config,
  lib,
  outputs,
  ...
}: {
  nix = {
    package = lib.mkForce pkgs.nixVersions.latest;
    settings = {
      experimental-features = ["nix-command" "flakes"];
      warn-dirty = false;
    };
  };

  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;

  home.file =
    lib.attrsets.concatMapAttrs
    (name: value: {
      ${name} = {
        target = "${config.xdg.userDirs.pictures}/walls/${name}.${value.ext}";
        source = value.src;
      };
    })
    outputs.wallpapers;

  home.preferXdgDirectories = true;

  xdg.userDirs = {
    enable =
      if pkgs.stdenv.isDarwin
      then false
      else true;
    createDirectories = true;
    setSessionVariables = false;

    # These are useless to me
    desktop = null;
    publicShare = null;
    templates = null;

    documents = "${config.home.homeDirectory}/docs";
    download = "${config.home.homeDirectory}/dl";
    music = "${config.home.homeDirectory}/music";
    pictures = "${config.home.homeDirectory}/pics";
    videos = "${config.home.homeDirectory}/videos";

    extraConfig = {
      PROJECTS = "${config.home.homeDirectory}/projects";
      GAMES = "${config.home.homeDirectory}/games";
      MAILS = "${config.home.homeDirectory}/mails";
    };
  };

  services = lib.optionalAttrs (pkgs.stdenv.isDarwin != true) {
    keybase.enable = true;
    kbfs = {
      enable = true;
      mountPoint = ".keybase";
    };
  };

  # TODO: Extract in dedicated module or bind it to specific services
  # TODO: For tools not supporting opt out by environment variable I need another way
  # https://github.com/beatcracker/toptout/blob/master/examples/toptout_bash
  home.sessionVariables = {
    AGENT_NO_ANALYTICS = "1";
    AITOOLSVSCODE_DISABLETELEMETRY = "1";
    ALIBUILD_NO_ANALYTICS = "1";
    ALLOW_UI_ANALYTICS = "false";
    ANALYTICS = "no";
    APOLLO_TELEMETRY_DISABLED = "1";
    APPCD_TELEMETRY = "0";
    ARDUINO_METRICS_ENABLED = "false";
    ARM_DISABLE_TERRAFORM_PARTNER_ID = "true";
    AUTOMAGICA_NO_TELEMETRY = "1";
    AUTOMATEDLAB_TELEMETRY_OPTIN = "0";
    AUTOMATEDLAB_TELEMETRY_OPTOUT = "1";
    AZURE_CORE_COLLECT_TELEMETRY = "0";
    BATECT_ENABLE_TELEMETRY = "false";
    BF_CLI_TELEMETRY = "false";
    BUGGER_OFF = "1";
    CANVAS_LMS_STATS_COLLECTION = "opt_out";
    CARBON_TELEMETRY_DISABLED = "1";
    CHECKPOINT_DISABLE = "1";
    CHEF_TELEMETRY_OPT_OUT = "1";
    CHOOSENIM_NO_ANALYTICS = "1";
    CI = "1";
    CLOUDSDK_CORE_DISABLE_USAGE_REPORTING = "true";
    COCOAPODS_DISABLE_STATS = "true";
    COLLECT_LEARNINGS_OPT_OUT = "true";
    CUBEJS_TELEMETRY = "false";
    DAGSTER_DISABLE_TELEMETRY = "1";
    DASH_DISABLE_TELEMETRY = "1";
    DA_TEST_DISABLE_TELEMETRY = "1";
    DECK_ANALYTICS = "off";
    DISABLE_AUTO_UPDATE = "true";
    DISABLE_CRASH_REPORT = "1";
    DISABLE_OPENCOLLECTIVE = "true";
    DISABLE_QUICKWIT_TELEMETRY = "1";
    DOTNET_CLI_TELEMETRY_OPTOUT = "true";
    DOTNET_INTERACTIVE_CLI_TELEMETRY_OPTOUT = "1";
    DOTNET_SVCUTIL_TELEMETRY_OPTOUT = "1";
    DO_NOT_TRACK = "1";
    DisableTelemetry = "True";
    EARTHLY_DISABLE_ANALYTICS = "1";
    ET_NO_TELEMETRY = "1";
    F5_ALLOW_TELEMETRY = "false";
    FASTLANE_OPT_OUT_USAGE = "YES";
    FEAST_TELEMETRY = "False";
    GATSBY_TELEMETRY_DISABLED = "1";
    GOTELEMETRY = "off";
    HASURA_GRAPHQL_ENABLE_TELEMETRY = "false";
    HF_HUB_DISABLE_TELEMETRY = "1";
    HF_HUB_OFFLINE = "1";
    HINT_TELEMETRY = "off";
    HOMEBREW_NO_ANALYTICS = "1";
    HOMEBREW_NO_ANALYTICS_THIS_RUN = "1";
    HOMEBREW_NO_AUTO_UPDATE = "1";
    HOOKDECK_CLI_TELEMETRY_OPTOUT = "1";
    IG_PRO_OPT_OUT = "YES";
    INFLUXD_REPORTING_DISABLED = "true";
    INFRACOST_SELF_HOSTED_TELEMETRY = "false";
    INFRACOST_SKIP_UPDATE_CHECK = "true";
    KICS_COLLECT_TELEMETRY = "0";
    LS_METRICS_HOST_ENABLED = "0";
    LYNX_ANALYTICS = "0";
    MEILI_NO_ANALYTICS = "true";
    MELTANO_DISABLE_TRACKING = "True";
    MLDOTNET_CLI_TELEMETRY_OPTOUT = "True";
    MM_LOGSETTINGS_ENABLEDIAGNOSTICS = "false";
    MM_SERVICESETTINGS_ENABLESECURITYFIXALERT = "false";
    MOBILE_CENTER_TELEMETRY = "off";
    MSLAB_TELEMETRY_LEVEL = "None";
    MSSQL_CLI_TELEMETRY_OPTOUT = "True";
    NC_DISABLE_TELE = "1";
    NEXT_TELEMETRY_DISABLED = "1";
    NG_CLI_ANALYTICS = "false";
    NG_CLI_ANALYTICS_SHARE = "false";
    NPM_CONFIG_UPDATE_NOTIFIER = "false";
    NUKE_TELEMETRY_OPTOUT = "1";
    NUXT_TELEMETRY_DISABLED = "1";
    ONE_CODEX_NO_TELEMETRY = "True";
    ORYX_DISABLE_TELEMETRY = "true";
    PANTS_ANONYMOUS_TELEMETRY_ENABLED = "false";
    PNPPOWERSHELL_DISABLETELEMETRY = "true";
    PNPPOWERSHELL_UPDATECHECK = "false";
    POWERSHELL_TELEMETRY_OPTOUT = "1";
    POWERSHELL_UPDATECHECK = "Off";
    PROSE_TELEMETRY_OPTOUT = "1";
    PULUMI_SKIP_UPDATE_CHECK = "true";
    QUILT_DISABLE_USAGE_METRICS = "True";
    RASA_TELEMETRY_ENABLED = "false";
    REACT_APP_WEBINY_TELEMETRY = "false";
    REDIRECT_TYPO3_DISABLE_CORE_UPDATER = "1";
    REPORTPORTAL_CLIENT_JS_NO_ANALYTICS = "true";
    RESTLER_TELEMETRY_OPTOUT = "1";
    ROCKSET_CLI_TELEMETRY_OPTOUT = "1";
    SALTO_TELEMETRY_DISABLE = "1";
    SAM_CLI_TELEMETRY = "0";
    SCOUT_DISABLE = "1";
    SEMGREP_SEND_METRICS = "off";
    SFDX_DISABLE_TELEMETRY = "true";
    SF_DISABLE_TELEMETRY = "true";
    SKU_TELEMETRY = "false";
    SLS_NOTIFICATIONS_MODE = "off";
    SLS_TELEMETRY_DISABLED = "1";
    SLS_TRACKING_DISABLED = "1";
    SQA_OPT_OUT = "true";
    STORYBOOK_DISABLE_TELEMETRY = "1";
    STRAPI_DISABLE_UPDATE_NOTIFICATION = "true";
    STRAPI_TELEMETRY_DISABLED = "true";
    STRIPE_CLI_TELEMETRY_OPTOUT = "1";
    SUGGESTIONS_OPT_OUT = "1";
    TEEM_DISABLE = "true";
    TELEMETRY_DISABLED = "1";
    TELEMETRY_ENABLED = "0";
    TUIST_STATS_OPT_OUT = "1";
    TYPO3_DISABLE_CORE_UPDATER = "1";
    VAGRANT_BOX_UPDATE_CHECK_DISABLE = "1";
    VAGRANT_CHECKPOINT_DISABLE = "1";
    VSTEST_TELEMETRY_OPTEDIN = "0";
    VUEDX_TELEMETRY = "off";
    WERF_TELEMETRY = "0";
    YARN_ENABLE_TELEMETRY = "0";
  };
}
