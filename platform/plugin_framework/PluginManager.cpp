
#include <string>
#include <boost/filesystem.hpp>
#include "PluginManager.h"
#include "DynamicLibrary.h"
#include "ObjectAdapter.h"

#if defined(PF_PLATFORM_MAC)
  static std::string dynamicLibraryExtension(".dylib");
#elif defined(PF_PLATFORM_LINUX)
  static std::string dynamicLibraryExtension(".so");
#elif defined(PF_PLATFORM_WINDOWS)
  static std::string dynamicLibraryExtension(".dll");
#endif

// The registration params may be received from an external plugin so it is 
// crucial to validate it, because it was never subjected to our tests.
static bool isValid(const char * objectType, const PF_RegisterParams * params)
{
  if (!objectType || !(*objectType))
     return false;
  if (!params ||!params->createFunc || !params->destroyFunc)
    return false;
  
  return true;
}

// ---------------------------------------------------------------

int32_t PluginManager::registerObject(const char * objectType, const PF_RegisterParams * params)
{
  // Check parameters
  if (!isValid(objectType, params))
    return -1;
 
  PluginManager & pm = PluginManager::getInstance();
  
  // Verify that versions match
  PF_PluginAPI_Version v = pm.platformServices_.version;
  if (v.major != params->version.major)
    return -1;
    
  std::string key((const char *)objectType);
  // If it's a wild card registration just add it
  if (key == std::string("*"))
  {
    pm.wildCardVec_.push_back(*params);
    return 0;
  }
  
  // If item already exists in eactMatc fail (only one can handle)
  if (pm.exactMatchMap_.find(key) != pm.exactMatchMap_.end())    
    return -1;
  
  pm.exactMatchMap_[key] = *params;
  return 0; 
}

// ---------------------------------------------------------------

PluginManager & PluginManager::getInstance()
{
  static PluginManager instance;
  
  return instance;
}


int32_t PluginManager::loadAll(const std::string & pluginDirectory, PF_InvokeServiceFunc func)
{
	if (pluginDirectory.empty()) // Check that the path is non-empty.
		return -1;

	platformServices_.invokeService = func;

	boost::filesystem::path dir_path(pluginDirectory);
	if (!boost::filesystem::exists(dir_path) || !boost::filesystem::is_directory(dir_path))
		return -1;

	boost::filesystem::directory_iterator dir_end;
	boost::filesystem::directory_iterator iter(dir_path);
	for( ; iter!=dir_end; ++iter)
	{
		// Skip directories
		if( boost::filesystem::is_directory(*iter) )
			continue;
		// Skip files with the wrong extension
		std::string  ext = boost::filesystem::extension(*iter);
		if( boost::filesystem::extension(*iter) != dynamicLibraryExtension )
			continue;
		// Ignore return value
		
		/*int32_t res = */ loadByPath(boost::filesystem::complete(*iter).file_string());
	}
	return 0;
}  

int32_t PluginManager::initializePlugin(PF_InitFunc initFunc)
{
  PluginManager & pm = PluginManager::getInstance();

  PF_ExitFunc exitFunc = initFunc(&pm.platformServices_);
  if (!exitFunc)
    return -1;
  
  // Store the exit func so it can be called when unloading this plugin
  pm.exitFuncVec_.push_back(exitFunc);
  return 0;
}
 
PluginManager::PluginManager() : inInitializePlugin_(false)
{
  platformServices_.version.major = 1;
  platformServices_.version.minor = 0;
  platformServices_.invokeService = NULL; // can be populated during loadAll()
  platformServices_.registerObject = registerObject;
}

PluginManager::~PluginManager()
{
  // Just in case it wasn't called earlier
  shutdown();
}

int32_t PluginManager::shutdown()
{
  int32_t result = 0;
  for (ExitFuncVec::iterator func = exitFuncVec_.begin(); func != exitFuncVec_.end(); ++func)
  {
    try
    {
      result = (*func)();
    }
    catch (...)
    {
      result = -1;
    }
  }
  
  
  dynamicLibraryMap_.clear();
  exactMatchMap_.clear();
  wildCardVec_.clear();
  exitFuncVec_.clear();
  
  return result;
}

int32_t PluginManager::loadByPath(const std::string & pluginPath)
{
	boost::filesystem::path path(pluginPath);
    
    // Resolve symbolic links
	
    #ifndef WIN32 
// TODO: Check handling of symlinks on *NIX system
    #endif
             
    // Don't load the same dynamic library twice
    if (dynamicLibraryMap_.find(path.file_string()) != dynamicLibraryMap_.end())
      return -1;

	path = boost::filesystem::complete(path);

    std::string errorString;
    DynamicLibrary * d = loadLibrary(std::string(path.file_string()), errorString);
    if (!d) // not a dynamic library? 
      return -1;
                    
    // Get the NTA_initPlugin() function
    PF_InitFunc initFunc = (PF_InitFunc)(d->getSymbol("PF_initPlugin"));
    if (!initFunc) // dynamic library missing entry point?
      return -1;
    
    int32_t res = initializePlugin(initFunc);
    if (res < 0) // failed to initalize?
      return res;
    
    return 0;
}


// ---------------------------------------------------------------

//template <typename T, typename U>
//T * PluginManager::createObject(const std::string & objectType, IObjectAdapter<T, U> & adapter)
//{
//  // "*" is not a valid object type
//  if (objectType == std::string("*"))
//    return NULL;
//  
//  // Prepare object params
//  PF_ObjectParams np;
//  np.objectType = objectType.c_str();
//  np.platformServices = &ServiceProvider::getInstance();
//
//  // Exact match found
//  if (exactMatchMap_.find(objectType) != exactMatchMap_.end())
//  {        
//    PF_RegisterParams & rp = exactMatchMap_[objectType];
//    IObject * object = createObject(rp, np, adapter);
//    if (object) // great, it worked
//      return object;
//  }
//  
//  for (Size i = 0; i < wildCardVec_.size(); ++i)
//  {
//    PF_RegisterParams & rp = wildCardVec_[i];
//    IObject * object = createObject(rp, np, adapter);
//    if (object) // great, it worked
//    {
//      // promote registration to exactMatc_ 
//      // (but keep also the wild card registration for other object types)
//      int32_t res = registerObject(np.objectType, &rp);
//      if (res < 0)
//      {
//        PF_THROW << "PluginManager::createObject(" << np.objectType << "), registration failed";
//        delete object;
//        return NULL;
//      }
//      return object;
//    }      
//  }
//
//  // Too bad no one can create this objectType
//  return NULL;
//}


// ---------------------------------------------------------------


void * PluginManager::createObject(const std::string & objectType, IObjectAdapter & adapter)
{
  // "*" is not a valid object type
  if (objectType == std::string("*"))
    return NULL;
  
  // Prepare object params
  PF_ObjectParams np;
  np.objectType = (const char *)objectType.c_str();
  np.platformServices = &platformServices_;

  // Exact match found
  if (exactMatchMap_.find(objectType) != exactMatchMap_.end())
  {        
    PF_RegisterParams & rp = exactMatchMap_[objectType];
    void * object = rp.createFunc(&np);
    if (object) // great, there is an exact match
    {
      // Adapt if necessary (wrap C objects using an adapter)
      if (rp.programmingLanguage == PF_ProgrammingLanguage_C)
        object = adapter.adapt(object, rp.destroyFunc);
        
      return object; 
    }
  }
  
  // Try to find a wild card match
  for (size_t i = 0; i < wildCardVec_.size(); ++i)
  {
    PF_RegisterParams & rp = wildCardVec_[i];
    void * object = rp.createFunc(&np);
    if (object) // great, it worked
    {
      // Adapt if necessary (wrap C objects using an adapter)
      if (rp.programmingLanguage == PF_ProgrammingLanguage_C)
        object = adapter.adapt(object, rp.destroyFunc);

      // promote registration to exactMatc_ 
      // (but keep also the wild card registration for other object types)
      int32_t res = registerObject(np.objectType, &rp);
      if (res < 0)
      {  
        // Serious framework should report or log it              
        rp.destroyFunc(object);
        return NULL;
      }
      return object;
    }      
  }

  // Too bad no one can create this objectType
  return NULL;
}

DynamicLibrary * PluginManager::loadLibrary(const std::string &  path, std::string & errorString)
{
    DynamicLibrary * d = DynamicLibrary::load(path, errorString);
    if (!d) // not a dynamic library? 
      return NULL;
     
    // Add library to map, so it can be unloaded
	dynamicLibraryMap_[boost::filesystem::complete(path).file_string()] = boost::shared_ptr<DynamicLibrary>(d);
    return d;
}

const PluginManager::RegistrationMap & PluginManager::getRegistrationMap()
{
  return exactMatchMap_;
}

PF_PlatformServices & PluginManager::getPlatformServices()
{
  return platformServices_;
}
