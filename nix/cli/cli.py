import fire
import subprocess
import os
from time import sleep


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
    cmd = [os.environ["RUN_GENESIS_SPO_SH"], config_folder, str(port)]
    subprocess.run(cmd)

def create_additional_utxo(config_folder):
    """
    additional utxo for genesis utxo for ctl collateral, create wallet.skey
    args:
        gen_config (bool) : genesis config folder  with db and node.socket
    """
    cmd = [os.environ["CREATE_ADDITIONAL_UTXO_SH"], config_folder]
    sleep(10)
    subprocess.run(cmd)


def run_staking_node(genesis_config_pth_or_url, node_config_path, port : int, access_point_url):
    """
    run staking node 
    args:
        genesis_config_pth_or_url : folder or web with genesis and configuration.yaml files
        node_config_path : path where necessary config will be generated for spo node 
        access_point_url : url for some other node to connect
        port 
    """
    cmd = [os.environ["RUN_STAKING_NODE_SH"],genesis_config_pth_or_url, node_config_path, str(port), access_point_url]
    subprocess.run(cmd)

def run_kupo(node_config_path):
    """
    run kupo
    args:
        node_config_path: folder with node metadata and socket 
    """
    cmd = [os.environ["RUN_KUPO_SH"], node_config_path]
    subprocess.run(cmd)

def run_ogmios(node_config_path):
    """
    run ogmios 
    args:
        node_config_path: folder with node metadata and socket 
    """
    cmd = [os.environ["RUN_OGMIOS_SH"], node_config_path]
    subprocess.run(cmd)

def run_prometheus(data, cardano_node_metrics_url, spammer_metrics_url):
    """
    run prometheus 
    args:
        data: where to put data, cardano_node_metrics_url (like 0.0.0.0:12789), spammer_metrics_url 
    """
    cmd = [os.environ["RUN_PROMETHEUS_SH"], data, cardano_node_metrics_url,  spammer_metrics_url ]
    subprocess.run(cmd)

def run_spammer(main_wallet_skey_path, ogmios_url, kupo_url):
    """
    run spammers on node, run faucet on port 8000, and await tx metrics on port 8001
    """
    os.environ["OGMIOS_URL"] = ogmios_url 
    os.environ["KUPO_URL"] = kupo_url 
    os.environ["WALLET_SKEY_PATH"] = main_wallet_skey_path 
    subprocess.run("spammer")


if __name__ == '__main__':
  fire.Fire()


