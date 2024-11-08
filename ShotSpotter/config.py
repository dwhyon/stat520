from pathlib import Path

PROJECT_ROOT = Path(__file__).parent
DATA_DIR = PROJECT_ROOT / "data"
DATA_DIR_RAW = DATA_DIR / "raw"
DATA_DIR_CLEAN = DATA_DIR / "clean"

COLUMN_DTYPES = {"incident_num": "str",
                 "date_time": "str",
                 "day_of_week": "int",
                 "address_number_primary": "int",
                 "address_dir_primary": "str",
                 "address_road_primary": "str",
                 "address_sfx_primary": "str",
                 "address_dir_intersecting": "float",
                 "address_road_intersecting": "str",
                 "address_sfx_intersecting": "float",
                 "call_type": "str",
                 "disposition": "str",
                 "beat": "int",
                 "priority": "int"}
