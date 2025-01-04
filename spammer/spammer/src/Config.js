export const getBackendEnvVars = () => {
    const result = {}
    result["ogmiosUrl"] = process.env.OGMIOS_URL
    result["kupoUrl"] = process.env.KUPO_URL
    result["walletPath"] = process.env.walletPath 
    return result
  }
