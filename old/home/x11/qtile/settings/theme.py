from os import path
import subprocess
import json

home       = path.expanduser('~')
qtile_path = path.join(home, ".config", "qtile")

def load_theme():
    theme = "onedark"

    config = path.join(qtile_path, "colors.json")
    if path.isfile(config):
            with open(config) as f:
                    theme = json.load(f)["theme"]
    else:
            with open(config, "w") as f:
                    f.write(f'{{"theme": "{theme}"}}\n')
                    
                    
    theme_file = path.join(qtile_path, "themes", f'{theme}.json')
    if not path.isfile(theme_file):
        raise Exception(f'"{theme_file}" does not exist')
    
    with open(path.join(theme_file)) as f:
        return json.load(f)

if __name__ == "settings.theme":
    colors = load_theme()
