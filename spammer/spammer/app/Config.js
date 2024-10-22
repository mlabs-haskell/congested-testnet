export const getEnvVars = () => {
    const result = {}
    result["ogmiosUrl"] = process.env.ogmiosUrl
    result["kupoUrl"] = process.env.kupoUrl
    result["walletPath"] = process.env.walletPath 
    return result
  }
