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


def run_staking_node(genesis_config_pth_or_url, node_config_path, port : int, access_point_url):
    """
    run staking node 
    args:
        genesis_config_pth_or_url : folder or web with genesis and configuration.yaml files
        node_config_path : path where necessary config will be generated for spo node 
        port 
    """
    cmd = [os.environ["GEN_STAKING_CONF_SH"], genesis_config_pth_or_url, node_config_path]
    subprocess.run(cmd)
    cmd = [os.environ["RUN_STAKING_NODE_SH"], node_config_path, str(port), access_point_url]
    subprocess.run(cmd)

def run_kupo(genesis_config_pth_or_url):
    """
    run staking node 
    args:
        genesis_config_pth_or_url : folder or web with genesis and configuration.yaml files
        node_config_path : path where necessary config will be generated for spo node 
        port 
    """
    cmd = [os.environ["RUN_KUPO_SH"], genesis_config_pth_or_url]
    subprocess.run(cmd)

def run_ogmios(genesis_config_pth_or_url):
    """
    run staking node 
    args:
        genesis_config_pth_or_url : folder or web with genesis and configuration.yaml files
        node_config_path : path where necessary config will be generated for spo node 
        port 
    """
    cmd = [os.environ["GEN_STAKING_CONF_SH"], genesis_config_pth_or_url, node_config_path]
    subprocess.run(cmd)
    cmd = [os.environ["RUN_STAKING_NODE_SH"], node_config_path, str(port), access_point_url]
    subprocess.run(cmd)

def run_spammer(genesis_config_pth_or_url):
    """
    run spammers on genesis node, local faucet (port 8000), ogmios (port 1337) and kupo (port 1442)
    args:
        genesis_config_pth_or_url : folder with genesis node metadata 
        node_config_path : path where necessary config will be generated for spo node 
        port 
    """
    cmd = [os.environ["RUN_OGMIOS_SH"], genesis_config_pth_or_url, node_config_path]
    subprocess.run(cmd)
    cmd = [os.environ["RUN_KUPO_SH"], genesis_config_pth_or_url, node_config_path]
    subprocess.run(cmd)


if __name__ == '__main__':
  fire.Fire()


