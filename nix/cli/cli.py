import fire
import subprocess
import os


def gen_testnet_conf(config_folder):
    """
    generate genesis config
    """
    GEN_TESTNET_CONF_SH = os.environ["GEN_TESTNET_CONF_SH"]
    cmd = [GEN_TESTNET_CONF_SH, config_folder]
    subprocess.run(cmd)


def run_genesis_spo(config_folder, port : int, gen_config = False):
    """
    run spo based on genesis config
    args:
        gen_config (bool) : if provided, generate genesis configuration 
    """
    if gen_config:
        gen_testnet_conf(config_folder)
    RUN_GENESIS_SPO_SH = os.environ["RUN_GENESIS_SPO_SH"]
    cmd = [RUN_GENESIS_SPO_SH, config_folder, str(port)]
    subprocess.run(cmd)


if __name__ == '__main__':
  fire.Fire()


