import fire
import subprocess
import os


def gen_testnet_conf(config_folder):
    """
    generate genesis config
    """
    
    GEN_TESTNET_CONF_SH = os.environ["GEN_TESTNET_CONF_SH"]
    cmd = [GEN_TESTNET_CONF_SH, config_folder]
    print(cmd)
    print(config_folder)
    subprocess.run(cmd)


if __name__ == '__main__':
  fire.Fire()


