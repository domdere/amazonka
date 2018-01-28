{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Product
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Product where

import Network.AWS.Greengrass.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Connectivity Info
--
-- /See:/ 'connectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { _ciPortNumber  :: !(Maybe Int)
  , _ciId          :: !(Maybe Text)
  , _ciMetadata    :: !(Maybe Text)
  , _ciHostAddress :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConnectivityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciPortNumber' - Port of the GGC. Usually 8883.
--
-- * 'ciId' - Element Id for this entry in the list.
--
-- * 'ciMetadata' - Metadata for this endpoint.
--
-- * 'ciHostAddress' - Endpoint for the GGC. Can be an IP address or DNS.
connectivityInfo
    :: ConnectivityInfo
connectivityInfo =
  ConnectivityInfo'
  { _ciPortNumber = Nothing
  , _ciId = Nothing
  , _ciMetadata = Nothing
  , _ciHostAddress = Nothing
  }


-- | Port of the GGC. Usually 8883.
ciPortNumber :: Lens' ConnectivityInfo (Maybe Int)
ciPortNumber = lens _ciPortNumber (\ s a -> s{_ciPortNumber = a});

-- | Element Id for this entry in the list.
ciId :: Lens' ConnectivityInfo (Maybe Text)
ciId = lens _ciId (\ s a -> s{_ciId = a});

-- | Metadata for this endpoint.
ciMetadata :: Lens' ConnectivityInfo (Maybe Text)
ciMetadata = lens _ciMetadata (\ s a -> s{_ciMetadata = a});

-- | Endpoint for the GGC. Can be an IP address or DNS.
ciHostAddress :: Lens' ConnectivityInfo (Maybe Text)
ciHostAddress = lens _ciHostAddress (\ s a -> s{_ciHostAddress = a});

instance FromJSON ConnectivityInfo where
        parseJSON
          = withObject "ConnectivityInfo"
              (\ x ->
                 ConnectivityInfo' <$>
                   (x .:? "PortNumber") <*> (x .:? "Id") <*>
                     (x .:? "Metadata")
                     <*> (x .:? "HostAddress"))

instance Hashable ConnectivityInfo where

instance NFData ConnectivityInfo where

instance ToJSON ConnectivityInfo where
        toJSON ConnectivityInfo'{..}
          = object
              (catMaybes
                 [("PortNumber" .=) <$> _ciPortNumber,
                  ("Id" .=) <$> _ciId, ("Metadata" .=) <$> _ciMetadata,
                  ("HostAddress" .=) <$> _ciHostAddress])

-- | Information on the core
--
-- /See:/ 'core' smart constructor.
data Core = Core'
  { _cCertificateARN :: !(Maybe Text)
  , _cThingARN       :: !(Maybe Text)
  , _cSyncShadow     :: !(Maybe Bool)
  , _cId             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Core' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateARN' - Certificate arn of the core.
--
-- * 'cThingARN' - Thing arn of the core.
--
-- * 'cSyncShadow' - If true, the local shadow value automatically syncs with the cloud's shadow state.
--
-- * 'cId' - Element Id for this entry in the list.
core
    :: Core
core =
  Core'
  { _cCertificateARN = Nothing
  , _cThingARN = Nothing
  , _cSyncShadow = Nothing
  , _cId = Nothing
  }


-- | Certificate arn of the core.
cCertificateARN :: Lens' Core (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a});

-- | Thing arn of the core.
cThingARN :: Lens' Core (Maybe Text)
cThingARN = lens _cThingARN (\ s a -> s{_cThingARN = a});

-- | If true, the local shadow value automatically syncs with the cloud's shadow state.
cSyncShadow :: Lens' Core (Maybe Bool)
cSyncShadow = lens _cSyncShadow (\ s a -> s{_cSyncShadow = a});

-- | Element Id for this entry in the list.
cId :: Lens' Core (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a});

instance FromJSON Core where
        parseJSON
          = withObject "Core"
              (\ x ->
                 Core' <$>
                   (x .:? "CertificateArn") <*> (x .:? "ThingArn") <*>
                     (x .:? "SyncShadow")
                     <*> (x .:? "Id"))

instance Hashable Core where

instance NFData Core where

instance ToJSON Core where
        toJSON Core'{..}
          = object
              (catMaybes
                 [("CertificateArn" .=) <$> _cCertificateARN,
                  ("ThingArn" .=) <$> _cThingARN,
                  ("SyncShadow" .=) <$> _cSyncShadow,
                  ("Id" .=) <$> _cId])

-- | Information on core definition version
--
-- /See:/ 'coreDefinitionVersion' smart constructor.
newtype CoreDefinitionVersion = CoreDefinitionVersion'
  { _cdvCores :: Maybe [Core]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvCores' - Cores in the definition version.
coreDefinitionVersion
    :: CoreDefinitionVersion
coreDefinitionVersion = CoreDefinitionVersion' {_cdvCores = Nothing}


-- | Cores in the definition version.
cdvCores :: Lens' CoreDefinitionVersion [Core]
cdvCores = lens _cdvCores (\ s a -> s{_cdvCores = a}) . _Default . _Coerce;

instance FromJSON CoreDefinitionVersion where
        parseJSON
          = withObject "CoreDefinitionVersion"
              (\ x ->
                 CoreDefinitionVersion' <$>
                   (x .:? "Cores" .!= mempty))

instance Hashable CoreDefinitionVersion where

instance NFData CoreDefinitionVersion where

instance ToJSON CoreDefinitionVersion where
        toJSON CoreDefinitionVersion'{..}
          = object (catMaybes [("Cores" .=) <$> _cdvCores])

-- | Information on the Definition
--
-- /See:/ 'definitionInformation' smart constructor.
data DefinitionInformation = DefinitionInformation'
  { _diLatestVersionARN     :: !(Maybe Text)
  , _diARN                  :: !(Maybe Text)
  , _diName                 :: !(Maybe Text)
  , _diCreationTimestamp    :: !(Maybe Text)
  , _diId                   :: !(Maybe Text)
  , _diLatestVersion        :: !(Maybe Text)
  , _diLastUpdatedTimestamp :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefinitionInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diLatestVersionARN' - Latest version arn of the definition.
--
-- * 'diARN' - Arn of the definition.
--
-- * 'diName' - Name of the definition.
--
-- * 'diCreationTimestamp' - Timestamp of when the definition was created.
--
-- * 'diId' - Id of the definition.
--
-- * 'diLatestVersion' - Last version of the definition.
--
-- * 'diLastUpdatedTimestamp' - Last updated timestamp of the definition.
definitionInformation
    :: DefinitionInformation
definitionInformation =
  DefinitionInformation'
  { _diLatestVersionARN = Nothing
  , _diARN = Nothing
  , _diName = Nothing
  , _diCreationTimestamp = Nothing
  , _diId = Nothing
  , _diLatestVersion = Nothing
  , _diLastUpdatedTimestamp = Nothing
  }


-- | Latest version arn of the definition.
diLatestVersionARN :: Lens' DefinitionInformation (Maybe Text)
diLatestVersionARN = lens _diLatestVersionARN (\ s a -> s{_diLatestVersionARN = a});

-- | Arn of the definition.
diARN :: Lens' DefinitionInformation (Maybe Text)
diARN = lens _diARN (\ s a -> s{_diARN = a});

-- | Name of the definition.
diName :: Lens' DefinitionInformation (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a});

-- | Timestamp of when the definition was created.
diCreationTimestamp :: Lens' DefinitionInformation (Maybe Text)
diCreationTimestamp = lens _diCreationTimestamp (\ s a -> s{_diCreationTimestamp = a});

-- | Id of the definition.
diId :: Lens' DefinitionInformation (Maybe Text)
diId = lens _diId (\ s a -> s{_diId = a});

-- | Last version of the definition.
diLatestVersion :: Lens' DefinitionInformation (Maybe Text)
diLatestVersion = lens _diLatestVersion (\ s a -> s{_diLatestVersion = a});

-- | Last updated timestamp of the definition.
diLastUpdatedTimestamp :: Lens' DefinitionInformation (Maybe Text)
diLastUpdatedTimestamp = lens _diLastUpdatedTimestamp (\ s a -> s{_diLastUpdatedTimestamp = a});

instance FromJSON DefinitionInformation where
        parseJSON
          = withObject "DefinitionInformation"
              (\ x ->
                 DefinitionInformation' <$>
                   (x .:? "LatestVersionArn") <*> (x .:? "Arn") <*>
                     (x .:? "Name")
                     <*> (x .:? "CreationTimestamp")
                     <*> (x .:? "Id")
                     <*> (x .:? "LatestVersion")
                     <*> (x .:? "LastUpdatedTimestamp"))

instance Hashable DefinitionInformation where

instance NFData DefinitionInformation where

-- | Information on the deployment
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dDeploymentId   :: !(Maybe Text)
  , _dDeploymentARN  :: !(Maybe Text)
  , _dCreatedAt      :: !(Maybe Text)
  , _dDeploymentType :: !(Maybe DeploymentType)
  , _dGroupARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - Id of the deployment.
--
-- * 'dDeploymentARN' - Arn of the deployment.
--
-- * 'dCreatedAt' - Timestamp when the deployment was created.
--
-- * 'dDeploymentType' - The type of deployment.
--
-- * 'dGroupARN' - Arn of the group for this deployment.
deployment
    :: Deployment
deployment =
  Deployment'
  { _dDeploymentId = Nothing
  , _dDeploymentARN = Nothing
  , _dCreatedAt = Nothing
  , _dDeploymentType = Nothing
  , _dGroupARN = Nothing
  }


-- | Id of the deployment.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\ s a -> s{_dDeploymentId = a});

-- | Arn of the deployment.
dDeploymentARN :: Lens' Deployment (Maybe Text)
dDeploymentARN = lens _dDeploymentARN (\ s a -> s{_dDeploymentARN = a});

-- | Timestamp when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a});

-- | The type of deployment.
dDeploymentType :: Lens' Deployment (Maybe DeploymentType)
dDeploymentType = lens _dDeploymentType (\ s a -> s{_dDeploymentType = a});

-- | Arn of the group for this deployment.
dGroupARN :: Lens' Deployment (Maybe Text)
dGroupARN = lens _dGroupARN (\ s a -> s{_dGroupARN = a});

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "DeploymentId") <*> (x .:? "DeploymentArn")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "DeploymentType")
                     <*> (x .:? "GroupArn"))

instance Hashable Deployment where

instance NFData Deployment where

-- | Information on a Device
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dCertificateARN :: !(Maybe Text)
  , _dThingARN       :: !(Maybe Text)
  , _dSyncShadow     :: !(Maybe Bool)
  , _dId             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateARN' - Certificate arn of the device.
--
-- * 'dThingARN' - Thing arn of the device.
--
-- * 'dSyncShadow' - If true, the local shadow value automatically syncs with the cloud's shadow state.
--
-- * 'dId' - Element Id for this entry in the list.
device
    :: Device
device =
  Device'
  { _dCertificateARN = Nothing
  , _dThingARN = Nothing
  , _dSyncShadow = Nothing
  , _dId = Nothing
  }


-- | Certificate arn of the device.
dCertificateARN :: Lens' Device (Maybe Text)
dCertificateARN = lens _dCertificateARN (\ s a -> s{_dCertificateARN = a});

-- | Thing arn of the device.
dThingARN :: Lens' Device (Maybe Text)
dThingARN = lens _dThingARN (\ s a -> s{_dThingARN = a});

-- | If true, the local shadow value automatically syncs with the cloud's shadow state.
dSyncShadow :: Lens' Device (Maybe Bool)
dSyncShadow = lens _dSyncShadow (\ s a -> s{_dSyncShadow = a});

-- | Element Id for this entry in the list.
dId :: Lens' Device (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a});

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "CertificateArn") <*> (x .:? "ThingArn") <*>
                     (x .:? "SyncShadow")
                     <*> (x .:? "Id"))

instance Hashable Device where

instance NFData Device where

instance ToJSON Device where
        toJSON Device'{..}
          = object
              (catMaybes
                 [("CertificateArn" .=) <$> _dCertificateARN,
                  ("ThingArn" .=) <$> _dThingARN,
                  ("SyncShadow" .=) <$> _dSyncShadow,
                  ("Id" .=) <$> _dId])

-- | Information on device definition version
--
-- /See:/ 'deviceDefinitionVersion' smart constructor.
newtype DeviceDefinitionVersion = DeviceDefinitionVersion'
  { _ddvDevices :: Maybe [Device]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvDevices' - Devices in the definition version.
deviceDefinitionVersion
    :: DeviceDefinitionVersion
deviceDefinitionVersion = DeviceDefinitionVersion' {_ddvDevices = Nothing}


-- | Devices in the definition version.
ddvDevices :: Lens' DeviceDefinitionVersion [Device]
ddvDevices = lens _ddvDevices (\ s a -> s{_ddvDevices = a}) . _Default . _Coerce;

instance FromJSON DeviceDefinitionVersion where
        parseJSON
          = withObject "DeviceDefinitionVersion"
              (\ x ->
                 DeviceDefinitionVersion' <$>
                   (x .:? "Devices" .!= mempty))

instance Hashable DeviceDefinitionVersion where

instance NFData DeviceDefinitionVersion where

instance ToJSON DeviceDefinitionVersion where
        toJSON DeviceDefinitionVersion'{..}
          = object (catMaybes [("Devices" .=) <$> _ddvDevices])

-- | ErrorDetail
--
-- /See:/ 'errorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { _edDetailedErrorCode    :: !(Maybe Text)
  , _edDetailedErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDetailedErrorCode' - Detailed Error Code
--
-- * 'edDetailedErrorMessage' - Detailed Error Message
errorDetail
    :: ErrorDetail
errorDetail =
  ErrorDetail'
  {_edDetailedErrorCode = Nothing, _edDetailedErrorMessage = Nothing}


-- | Detailed Error Code
edDetailedErrorCode :: Lens' ErrorDetail (Maybe Text)
edDetailedErrorCode = lens _edDetailedErrorCode (\ s a -> s{_edDetailedErrorCode = a});

-- | Detailed Error Message
edDetailedErrorMessage :: Lens' ErrorDetail (Maybe Text)
edDetailedErrorMessage = lens _edDetailedErrorMessage (\ s a -> s{_edDetailedErrorMessage = a});

instance FromJSON ErrorDetail where
        parseJSON
          = withObject "ErrorDetail"
              (\ x ->
                 ErrorDetail' <$>
                   (x .:? "DetailedErrorCode") <*>
                     (x .:? "DetailedErrorMessage"))

instance Hashable ErrorDetail where

instance NFData ErrorDetail where

-- | Information on function
--
-- /See:/ 'function' smart constructor.
data Function = Function'
  { _fFunctionARN           :: !(Maybe Text)
  , _fFunctionConfiguration :: !(Maybe FunctionConfiguration)
  , _fId                    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Function' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFunctionARN' - Arn of the Lambda function.
--
-- * 'fFunctionConfiguration' - Configuration of the function
--
-- * 'fId' - Id of the function in this version.
function
    :: Function
function =
  Function'
  {_fFunctionARN = Nothing, _fFunctionConfiguration = Nothing, _fId = Nothing}


-- | Arn of the Lambda function.
fFunctionARN :: Lens' Function (Maybe Text)
fFunctionARN = lens _fFunctionARN (\ s a -> s{_fFunctionARN = a});

-- | Configuration of the function
fFunctionConfiguration :: Lens' Function (Maybe FunctionConfiguration)
fFunctionConfiguration = lens _fFunctionConfiguration (\ s a -> s{_fFunctionConfiguration = a});

-- | Id of the function in this version.
fId :: Lens' Function (Maybe Text)
fId = lens _fId (\ s a -> s{_fId = a});

instance FromJSON Function where
        parseJSON
          = withObject "Function"
              (\ x ->
                 Function' <$>
                   (x .:? "FunctionArn") <*>
                     (x .:? "FunctionConfiguration")
                     <*> (x .:? "Id"))

instance Hashable Function where

instance NFData Function where

instance ToJSON Function where
        toJSON Function'{..}
          = object
              (catMaybes
                 [("FunctionArn" .=) <$> _fFunctionARN,
                  ("FunctionConfiguration" .=) <$>
                    _fFunctionConfiguration,
                  ("Id" .=) <$> _fId])

-- | Configuration of the function
--
-- /See:/ 'functionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { _fcMemorySize  :: !(Maybe Int)
  , _fcExecArgs    :: !(Maybe Text)
  , _fcEnvironment :: !(Maybe FunctionConfigurationEnvironment)
  , _fcExecutable  :: !(Maybe Text)
  , _fcPinned      :: !(Maybe Bool)
  , _fcTimeout     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcMemorySize' - The memory size, in KB, you configured for the function.
--
-- * 'fcExecArgs' - Execution Arguments
--
-- * 'fcEnvironment' - Environment of the function configuration
--
-- * 'fcExecutable' - Executable
--
-- * 'fcPinned' - Whether the function is pinned or not. Pinned means the function is long-lived and starts when the core starts.
--
-- * 'fcTimeout' - The function execution time at which Lambda should terminate the function. This timeout still applies to pinned lambdas for each request.
functionConfiguration
    :: FunctionConfiguration
functionConfiguration =
  FunctionConfiguration'
  { _fcMemorySize = Nothing
  , _fcExecArgs = Nothing
  , _fcEnvironment = Nothing
  , _fcExecutable = Nothing
  , _fcPinned = Nothing
  , _fcTimeout = Nothing
  }


-- | The memory size, in KB, you configured for the function.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Int)
fcMemorySize = lens _fcMemorySize (\ s a -> s{_fcMemorySize = a});

-- | Execution Arguments
fcExecArgs :: Lens' FunctionConfiguration (Maybe Text)
fcExecArgs = lens _fcExecArgs (\ s a -> s{_fcExecArgs = a});

-- | Environment of the function configuration
fcEnvironment :: Lens' FunctionConfiguration (Maybe FunctionConfigurationEnvironment)
fcEnvironment = lens _fcEnvironment (\ s a -> s{_fcEnvironment = a});

-- | Executable
fcExecutable :: Lens' FunctionConfiguration (Maybe Text)
fcExecutable = lens _fcExecutable (\ s a -> s{_fcExecutable = a});

-- | Whether the function is pinned or not. Pinned means the function is long-lived and starts when the core starts.
fcPinned :: Lens' FunctionConfiguration (Maybe Bool)
fcPinned = lens _fcPinned (\ s a -> s{_fcPinned = a});

-- | The function execution time at which Lambda should terminate the function. This timeout still applies to pinned lambdas for each request.
fcTimeout :: Lens' FunctionConfiguration (Maybe Int)
fcTimeout = lens _fcTimeout (\ s a -> s{_fcTimeout = a});

instance FromJSON FunctionConfiguration where
        parseJSON
          = withObject "FunctionConfiguration"
              (\ x ->
                 FunctionConfiguration' <$>
                   (x .:? "MemorySize") <*> (x .:? "ExecArgs") <*>
                     (x .:? "Environment")
                     <*> (x .:? "Executable")
                     <*> (x .:? "Pinned")
                     <*> (x .:? "Timeout"))

instance Hashable FunctionConfiguration where

instance NFData FunctionConfiguration where

instance ToJSON FunctionConfiguration where
        toJSON FunctionConfiguration'{..}
          = object
              (catMaybes
                 [("MemorySize" .=) <$> _fcMemorySize,
                  ("ExecArgs" .=) <$> _fcExecArgs,
                  ("Environment" .=) <$> _fcEnvironment,
                  ("Executable" .=) <$> _fcExecutable,
                  ("Pinned" .=) <$> _fcPinned,
                  ("Timeout" .=) <$> _fcTimeout])

-- | Environment of the function configuration
--
-- /See:/ 'functionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { _fceVariables              :: !(Maybe (Map Text Text))
  , _fceResourceAccessPolicies :: !(Maybe [ResourceAccessPolicy])
  , _fceAccessSysfs            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionConfigurationEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fceVariables' - Environment variables for the lambda function.
--
-- * 'fceResourceAccessPolicies' - Policies for the function to access resources.
--
-- * 'fceAccessSysfs' - Flag to allow lambda access sys filesystem.
functionConfigurationEnvironment
    :: FunctionConfigurationEnvironment
functionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
  { _fceVariables = Nothing
  , _fceResourceAccessPolicies = Nothing
  , _fceAccessSysfs = Nothing
  }


-- | Environment variables for the lambda function.
fceVariables :: Lens' FunctionConfigurationEnvironment (HashMap Text Text)
fceVariables = lens _fceVariables (\ s a -> s{_fceVariables = a}) . _Default . _Map;

-- | Policies for the function to access resources.
fceResourceAccessPolicies :: Lens' FunctionConfigurationEnvironment [ResourceAccessPolicy]
fceResourceAccessPolicies = lens _fceResourceAccessPolicies (\ s a -> s{_fceResourceAccessPolicies = a}) . _Default . _Coerce;

-- | Flag to allow lambda access sys filesystem.
fceAccessSysfs :: Lens' FunctionConfigurationEnvironment (Maybe Bool)
fceAccessSysfs = lens _fceAccessSysfs (\ s a -> s{_fceAccessSysfs = a});

instance FromJSON FunctionConfigurationEnvironment
         where
        parseJSON
          = withObject "FunctionConfigurationEnvironment"
              (\ x ->
                 FunctionConfigurationEnvironment' <$>
                   (x .:? "Variables" .!= mempty) <*>
                     (x .:? "ResourceAccessPolicies" .!= mempty)
                     <*> (x .:? "AccessSysfs"))

instance Hashable FunctionConfigurationEnvironment
         where

instance NFData FunctionConfigurationEnvironment
         where

instance ToJSON FunctionConfigurationEnvironment
         where
        toJSON FunctionConfigurationEnvironment'{..}
          = object
              (catMaybes
                 [("Variables" .=) <$> _fceVariables,
                  ("ResourceAccessPolicies" .=) <$>
                    _fceResourceAccessPolicies,
                  ("AccessSysfs" .=) <$> _fceAccessSysfs])

-- | Information on the function definition version
--
-- /See:/ 'functionDefinitionVersion' smart constructor.
newtype FunctionDefinitionVersion = FunctionDefinitionVersion'
  { _fdvFunctions :: Maybe [Function]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdvFunctions' - Lambda functions in this function definition version.
functionDefinitionVersion
    :: FunctionDefinitionVersion
functionDefinitionVersion = FunctionDefinitionVersion' {_fdvFunctions = Nothing}


-- | Lambda functions in this function definition version.
fdvFunctions :: Lens' FunctionDefinitionVersion [Function]
fdvFunctions = lens _fdvFunctions (\ s a -> s{_fdvFunctions = a}) . _Default . _Coerce;

instance FromJSON FunctionDefinitionVersion where
        parseJSON
          = withObject "FunctionDefinitionVersion"
              (\ x ->
                 FunctionDefinitionVersion' <$>
                   (x .:? "Functions" .!= mempty))

instance Hashable FunctionDefinitionVersion where

instance NFData FunctionDefinitionVersion where

instance ToJSON FunctionDefinitionVersion where
        toJSON FunctionDefinitionVersion'{..}
          = object
              (catMaybes [("Functions" .=) <$> _fdvFunctions])

-- | Information on the Logger
--
-- /See:/ 'greengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
  { _glSpace     :: !(Maybe Int)
  , _glComponent :: !(Maybe LoggerComponent)
  , _glId        :: !(Maybe Text)
  , _glType      :: !(Maybe LoggerType)
  , _glLevel     :: !(Maybe LoggerLevel)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GreengrassLogger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glSpace' - Amount of hardware space, in KB, to use if file system is used for logging purposes.
--
-- * 'glComponent' - The component that will be subject to logs
--
-- * 'glId' - Element Id for this entry in the list.
--
-- * 'glType' - The type which will be use for log output
--
-- * 'glLevel' - The level of the logs
greengrassLogger
    :: GreengrassLogger
greengrassLogger =
  GreengrassLogger'
  { _glSpace = Nothing
  , _glComponent = Nothing
  , _glId = Nothing
  , _glType = Nothing
  , _glLevel = Nothing
  }


-- | Amount of hardware space, in KB, to use if file system is used for logging purposes.
glSpace :: Lens' GreengrassLogger (Maybe Int)
glSpace = lens _glSpace (\ s a -> s{_glSpace = a});

-- | The component that will be subject to logs
glComponent :: Lens' GreengrassLogger (Maybe LoggerComponent)
glComponent = lens _glComponent (\ s a -> s{_glComponent = a});

-- | Element Id for this entry in the list.
glId :: Lens' GreengrassLogger (Maybe Text)
glId = lens _glId (\ s a -> s{_glId = a});

-- | The type which will be use for log output
glType :: Lens' GreengrassLogger (Maybe LoggerType)
glType = lens _glType (\ s a -> s{_glType = a});

-- | The level of the logs
glLevel :: Lens' GreengrassLogger (Maybe LoggerLevel)
glLevel = lens _glLevel (\ s a -> s{_glLevel = a});

instance FromJSON GreengrassLogger where
        parseJSON
          = withObject "GreengrassLogger"
              (\ x ->
                 GreengrassLogger' <$>
                   (x .:? "Space") <*> (x .:? "Component") <*>
                     (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Level"))

instance Hashable GreengrassLogger where

instance NFData GreengrassLogger where

instance ToJSON GreengrassLogger where
        toJSON GreengrassLogger'{..}
          = object
              (catMaybes
                 [("Space" .=) <$> _glSpace,
                  ("Component" .=) <$> _glComponent,
                  ("Id" .=) <$> _glId, ("Type" .=) <$> _glType,
                  ("Level" .=) <$> _glLevel])

-- | Information on group certificate authority properties
--
-- /See:/ 'groupCertificateAuthorityProperties' smart constructor.
data GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties'
  { _gcapGroupCertificateAuthorityARN :: !(Maybe Text)
  , _gcapGroupCertificateAuthorityId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupCertificateAuthorityProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcapGroupCertificateAuthorityARN' - Arn of the certificate authority for the group.
--
-- * 'gcapGroupCertificateAuthorityId' - Id of the certificate authority for the group.
groupCertificateAuthorityProperties
    :: GroupCertificateAuthorityProperties
groupCertificateAuthorityProperties =
  GroupCertificateAuthorityProperties'
  { _gcapGroupCertificateAuthorityARN = Nothing
  , _gcapGroupCertificateAuthorityId = Nothing
  }


-- | Arn of the certificate authority for the group.
gcapGroupCertificateAuthorityARN :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityARN = lens _gcapGroupCertificateAuthorityARN (\ s a -> s{_gcapGroupCertificateAuthorityARN = a});

-- | Id of the certificate authority for the group.
gcapGroupCertificateAuthorityId :: Lens' GroupCertificateAuthorityProperties (Maybe Text)
gcapGroupCertificateAuthorityId = lens _gcapGroupCertificateAuthorityId (\ s a -> s{_gcapGroupCertificateAuthorityId = a});

instance FromJSON GroupCertificateAuthorityProperties
         where
        parseJSON
          = withObject "GroupCertificateAuthorityProperties"
              (\ x ->
                 GroupCertificateAuthorityProperties' <$>
                   (x .:? "GroupCertificateAuthorityArn") <*>
                     (x .:? "GroupCertificateAuthorityId"))

instance Hashable GroupCertificateAuthorityProperties
         where

instance NFData GroupCertificateAuthorityProperties
         where

-- | Information on the group
--
-- /See:/ 'groupInformation' smart constructor.
data GroupInformation = GroupInformation'
  { _giLatestVersionARN     :: !(Maybe Text)
  , _giARN                  :: !(Maybe Text)
  , _giName                 :: !(Maybe Text)
  , _giCreationTimestamp    :: !(Maybe Text)
  , _giId                   :: !(Maybe Text)
  , _giLatestVersion        :: !(Maybe Text)
  , _giLastUpdatedTimestamp :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giLatestVersionARN' - Latest version arn of the group.
--
-- * 'giARN' - Arn of a group.
--
-- * 'giName' - Name of a group.
--
-- * 'giCreationTimestamp' - Timestamp of when the group was created.
--
-- * 'giId' - Id of a group.
--
-- * 'giLatestVersion' - Last version of the group.
--
-- * 'giLastUpdatedTimestamp' - Last updated timestamp of the group.
groupInformation
    :: GroupInformation
groupInformation =
  GroupInformation'
  { _giLatestVersionARN = Nothing
  , _giARN = Nothing
  , _giName = Nothing
  , _giCreationTimestamp = Nothing
  , _giId = Nothing
  , _giLatestVersion = Nothing
  , _giLastUpdatedTimestamp = Nothing
  }


-- | Latest version arn of the group.
giLatestVersionARN :: Lens' GroupInformation (Maybe Text)
giLatestVersionARN = lens _giLatestVersionARN (\ s a -> s{_giLatestVersionARN = a});

-- | Arn of a group.
giARN :: Lens' GroupInformation (Maybe Text)
giARN = lens _giARN (\ s a -> s{_giARN = a});

-- | Name of a group.
giName :: Lens' GroupInformation (Maybe Text)
giName = lens _giName (\ s a -> s{_giName = a});

-- | Timestamp of when the group was created.
giCreationTimestamp :: Lens' GroupInformation (Maybe Text)
giCreationTimestamp = lens _giCreationTimestamp (\ s a -> s{_giCreationTimestamp = a});

-- | Id of a group.
giId :: Lens' GroupInformation (Maybe Text)
giId = lens _giId (\ s a -> s{_giId = a});

-- | Last version of the group.
giLatestVersion :: Lens' GroupInformation (Maybe Text)
giLatestVersion = lens _giLatestVersion (\ s a -> s{_giLatestVersion = a});

-- | Last updated timestamp of the group.
giLastUpdatedTimestamp :: Lens' GroupInformation (Maybe Text)
giLastUpdatedTimestamp = lens _giLastUpdatedTimestamp (\ s a -> s{_giLastUpdatedTimestamp = a});

instance FromJSON GroupInformation where
        parseJSON
          = withObject "GroupInformation"
              (\ x ->
                 GroupInformation' <$>
                   (x .:? "LatestVersionArn") <*> (x .:? "Arn") <*>
                     (x .:? "Name")
                     <*> (x .:? "CreationTimestamp")
                     <*> (x .:? "Id")
                     <*> (x .:? "LatestVersion")
                     <*> (x .:? "LastUpdatedTimestamp"))

instance Hashable GroupInformation where

instance NFData GroupInformation where

-- | Group owner related settings for local resources.
--
-- /See:/ 'groupOwnerSetting' smart constructor.
data GroupOwnerSetting = GroupOwnerSetting'
  { _gosAutoAddGroupOwner :: !(Maybe Bool)
  , _gosGroupOwner        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupOwnerSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosAutoAddGroupOwner' - Eanble the auto added group owner.
--
-- * 'gosGroupOwner' - Name of the group owner.
groupOwnerSetting
    :: GroupOwnerSetting
groupOwnerSetting =
  GroupOwnerSetting' {_gosAutoAddGroupOwner = Nothing, _gosGroupOwner = Nothing}


-- | Eanble the auto added group owner.
gosAutoAddGroupOwner :: Lens' GroupOwnerSetting (Maybe Bool)
gosAutoAddGroupOwner = lens _gosAutoAddGroupOwner (\ s a -> s{_gosAutoAddGroupOwner = a});

-- | Name of the group owner.
gosGroupOwner :: Lens' GroupOwnerSetting (Maybe Text)
gosGroupOwner = lens _gosGroupOwner (\ s a -> s{_gosGroupOwner = a});

instance FromJSON GroupOwnerSetting where
        parseJSON
          = withObject "GroupOwnerSetting"
              (\ x ->
                 GroupOwnerSetting' <$>
                   (x .:? "AutoAddGroupOwner") <*> (x .:? "GroupOwner"))

instance Hashable GroupOwnerSetting where

instance NFData GroupOwnerSetting where

instance ToJSON GroupOwnerSetting where
        toJSON GroupOwnerSetting'{..}
          = object
              (catMaybes
                 [("AutoAddGroupOwner" .=) <$> _gosAutoAddGroupOwner,
                  ("GroupOwner" .=) <$> _gosGroupOwner])

-- | Information on group version
--
-- /See:/ 'groupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { _gvResourceDefinitionVersionARN     :: !(Maybe Text)
  , _gvSubscriptionDefinitionVersionARN :: !(Maybe Text)
  , _gvCoreDefinitionVersionARN         :: !(Maybe Text)
  , _gvDeviceDefinitionVersionARN       :: !(Maybe Text)
  , _gvFunctionDefinitionVersionARN     :: !(Maybe Text)
  , _gvLoggerDefinitionVersionARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvResourceDefinitionVersionARN' - Resource definition version arn for this group.
--
-- * 'gvSubscriptionDefinitionVersionARN' - Subscription definition version arn for this group.
--
-- * 'gvCoreDefinitionVersionARN' - Core definition version arn for this group.
--
-- * 'gvDeviceDefinitionVersionARN' - Device definition version arn for this group.
--
-- * 'gvFunctionDefinitionVersionARN' - Function definition version arn for this group.
--
-- * 'gvLoggerDefinitionVersionARN' - Logger definition version arn for this group.
groupVersion
    :: GroupVersion
groupVersion =
  GroupVersion'
  { _gvResourceDefinitionVersionARN = Nothing
  , _gvSubscriptionDefinitionVersionARN = Nothing
  , _gvCoreDefinitionVersionARN = Nothing
  , _gvDeviceDefinitionVersionARN = Nothing
  , _gvFunctionDefinitionVersionARN = Nothing
  , _gvLoggerDefinitionVersionARN = Nothing
  }


-- | Resource definition version arn for this group.
gvResourceDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvResourceDefinitionVersionARN = lens _gvResourceDefinitionVersionARN (\ s a -> s{_gvResourceDefinitionVersionARN = a});

-- | Subscription definition version arn for this group.
gvSubscriptionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvSubscriptionDefinitionVersionARN = lens _gvSubscriptionDefinitionVersionARN (\ s a -> s{_gvSubscriptionDefinitionVersionARN = a});

-- | Core definition version arn for this group.
gvCoreDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvCoreDefinitionVersionARN = lens _gvCoreDefinitionVersionARN (\ s a -> s{_gvCoreDefinitionVersionARN = a});

-- | Device definition version arn for this group.
gvDeviceDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvDeviceDefinitionVersionARN = lens _gvDeviceDefinitionVersionARN (\ s a -> s{_gvDeviceDefinitionVersionARN = a});

-- | Function definition version arn for this group.
gvFunctionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvFunctionDefinitionVersionARN = lens _gvFunctionDefinitionVersionARN (\ s a -> s{_gvFunctionDefinitionVersionARN = a});

-- | Logger definition version arn for this group.
gvLoggerDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvLoggerDefinitionVersionARN = lens _gvLoggerDefinitionVersionARN (\ s a -> s{_gvLoggerDefinitionVersionARN = a});

instance FromJSON GroupVersion where
        parseJSON
          = withObject "GroupVersion"
              (\ x ->
                 GroupVersion' <$>
                   (x .:? "ResourceDefinitionVersionArn") <*>
                     (x .:? "SubscriptionDefinitionVersionArn")
                     <*> (x .:? "CoreDefinitionVersionArn")
                     <*> (x .:? "DeviceDefinitionVersionArn")
                     <*> (x .:? "FunctionDefinitionVersionArn")
                     <*> (x .:? "LoggerDefinitionVersionArn"))

instance Hashable GroupVersion where

instance NFData GroupVersion where

instance ToJSON GroupVersion where
        toJSON GroupVersion'{..}
          = object
              (catMaybes
                 [("ResourceDefinitionVersionArn" .=) <$>
                    _gvResourceDefinitionVersionARN,
                  ("SubscriptionDefinitionVersionArn" .=) <$>
                    _gvSubscriptionDefinitionVersionARN,
                  ("CoreDefinitionVersionArn" .=) <$>
                    _gvCoreDefinitionVersionARN,
                  ("DeviceDefinitionVersionArn" .=) <$>
                    _gvDeviceDefinitionVersionARN,
                  ("FunctionDefinitionVersionArn" .=) <$>
                    _gvFunctionDefinitionVersionARN,
                  ("LoggerDefinitionVersionArn" .=) <$>
                    _gvLoggerDefinitionVersionARN])

-- | Attributes that define the Local Device Resource.
--
-- /See:/ 'localDeviceResourceData' smart constructor.
data LocalDeviceResourceData = LocalDeviceResourceData'
  { _ldrdGroupOwnerSetting :: !(Maybe GroupOwnerSetting)
  , _ldrdSourcePath        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalDeviceResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrdGroupOwnerSetting' - Group owner related settings for local resources.
--
-- * 'ldrdSourcePath' - Local source path of the resource.
localDeviceResourceData
    :: LocalDeviceResourceData
localDeviceResourceData =
  LocalDeviceResourceData'
  {_ldrdGroupOwnerSetting = Nothing, _ldrdSourcePath = Nothing}


-- | Group owner related settings for local resources.
ldrdGroupOwnerSetting :: Lens' LocalDeviceResourceData (Maybe GroupOwnerSetting)
ldrdGroupOwnerSetting = lens _ldrdGroupOwnerSetting (\ s a -> s{_ldrdGroupOwnerSetting = a});

-- | Local source path of the resource.
ldrdSourcePath :: Lens' LocalDeviceResourceData (Maybe Text)
ldrdSourcePath = lens _ldrdSourcePath (\ s a -> s{_ldrdSourcePath = a});

instance FromJSON LocalDeviceResourceData where
        parseJSON
          = withObject "LocalDeviceResourceData"
              (\ x ->
                 LocalDeviceResourceData' <$>
                   (x .:? "GroupOwnerSetting") <*> (x .:? "SourcePath"))

instance Hashable LocalDeviceResourceData where

instance NFData LocalDeviceResourceData where

instance ToJSON LocalDeviceResourceData where
        toJSON LocalDeviceResourceData'{..}
          = object
              (catMaybes
                 [("GroupOwnerSetting" .=) <$> _ldrdGroupOwnerSetting,
                  ("SourcePath" .=) <$> _ldrdSourcePath])

-- | Attributes that define the Local Volume Resource.
--
-- /See:/ 'localVolumeResourceData' smart constructor.
data LocalVolumeResourceData = LocalVolumeResourceData'
  { _lvrdGroupOwnerSetting :: !(Maybe GroupOwnerSetting)
  , _lvrdDestinationPath   :: !(Maybe Text)
  , _lvrdSourcePath        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LocalVolumeResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrdGroupOwnerSetting' - Group owner related settings for local resources.
--
-- * 'lvrdDestinationPath' - Local destination path of the resource.
--
-- * 'lvrdSourcePath' - Local source path of the resource.
localVolumeResourceData
    :: LocalVolumeResourceData
localVolumeResourceData =
  LocalVolumeResourceData'
  { _lvrdGroupOwnerSetting = Nothing
  , _lvrdDestinationPath = Nothing
  , _lvrdSourcePath = Nothing
  }


-- | Group owner related settings for local resources.
lvrdGroupOwnerSetting :: Lens' LocalVolumeResourceData (Maybe GroupOwnerSetting)
lvrdGroupOwnerSetting = lens _lvrdGroupOwnerSetting (\ s a -> s{_lvrdGroupOwnerSetting = a});

-- | Local destination path of the resource.
lvrdDestinationPath :: Lens' LocalVolumeResourceData (Maybe Text)
lvrdDestinationPath = lens _lvrdDestinationPath (\ s a -> s{_lvrdDestinationPath = a});

-- | Local source path of the resource.
lvrdSourcePath :: Lens' LocalVolumeResourceData (Maybe Text)
lvrdSourcePath = lens _lvrdSourcePath (\ s a -> s{_lvrdSourcePath = a});

instance FromJSON LocalVolumeResourceData where
        parseJSON
          = withObject "LocalVolumeResourceData"
              (\ x ->
                 LocalVolumeResourceData' <$>
                   (x .:? "GroupOwnerSetting") <*>
                     (x .:? "DestinationPath")
                     <*> (x .:? "SourcePath"))

instance Hashable LocalVolumeResourceData where

instance NFData LocalVolumeResourceData where

instance ToJSON LocalVolumeResourceData where
        toJSON LocalVolumeResourceData'{..}
          = object
              (catMaybes
                 [("GroupOwnerSetting" .=) <$> _lvrdGroupOwnerSetting,
                  ("DestinationPath" .=) <$> _lvrdDestinationPath,
                  ("SourcePath" .=) <$> _lvrdSourcePath])

-- | Information on logger definition version
--
-- /See:/ 'loggerDefinitionVersion' smart constructor.
newtype LoggerDefinitionVersion = LoggerDefinitionVersion'
  { _ldvLoggers :: Maybe [GreengrassLogger]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldvLoggers' - List of loggers.
loggerDefinitionVersion
    :: LoggerDefinitionVersion
loggerDefinitionVersion = LoggerDefinitionVersion' {_ldvLoggers = Nothing}


-- | List of loggers.
ldvLoggers :: Lens' LoggerDefinitionVersion [GreengrassLogger]
ldvLoggers = lens _ldvLoggers (\ s a -> s{_ldvLoggers = a}) . _Default . _Coerce;

instance FromJSON LoggerDefinitionVersion where
        parseJSON
          = withObject "LoggerDefinitionVersion"
              (\ x ->
                 LoggerDefinitionVersion' <$>
                   (x .:? "Loggers" .!= mempty))

instance Hashable LoggerDefinitionVersion where

instance NFData LoggerDefinitionVersion where

instance ToJSON LoggerDefinitionVersion where
        toJSON LoggerDefinitionVersion'{..}
          = object (catMaybes [("Loggers" .=) <$> _ldvLoggers])

-- | Information on the resource.
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rResourceDataContainer :: !(Maybe ResourceDataContainer)
  , _rName                  :: !(Maybe Text)
  , _rId                    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceDataContainer' - A container of data for all resource types.
--
-- * 'rName' - A descriptive resource name.
--
-- * 'rId' - Resource Id.
resource
    :: Resource
resource =
  Resource'
  {_rResourceDataContainer = Nothing, _rName = Nothing, _rId = Nothing}


-- | A container of data for all resource types.
rResourceDataContainer :: Lens' Resource (Maybe ResourceDataContainer)
rResourceDataContainer = lens _rResourceDataContainer (\ s a -> s{_rResourceDataContainer = a});

-- | A descriptive resource name.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a});

-- | Resource Id.
rId :: Lens' Resource (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a});

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "ResourceDataContainer") <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable Resource where

instance NFData Resource where

instance ToJSON Resource where
        toJSON Resource'{..}
          = object
              (catMaybes
                 [("ResourceDataContainer" .=) <$>
                    _rResourceDataContainer,
                  ("Name" .=) <$> _rName, ("Id" .=) <$> _rId])

-- | Policy for the function to access a resource.
--
-- /See:/ 'resourceAccessPolicy' smart constructor.
data ResourceAccessPolicy = ResourceAccessPolicy'
  { _rapResourceId :: !(Maybe Text)
  , _rapPermission :: !(Maybe Permission)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rapResourceId' - Id of the resource. A reference to the resource definiton.
--
-- * 'rapPermission' - The function's access permission to the resource.
resourceAccessPolicy
    :: ResourceAccessPolicy
resourceAccessPolicy =
  ResourceAccessPolicy' {_rapResourceId = Nothing, _rapPermission = Nothing}


-- | Id of the resource. A reference to the resource definiton.
rapResourceId :: Lens' ResourceAccessPolicy (Maybe Text)
rapResourceId = lens _rapResourceId (\ s a -> s{_rapResourceId = a});

-- | The function's access permission to the resource.
rapPermission :: Lens' ResourceAccessPolicy (Maybe Permission)
rapPermission = lens _rapPermission (\ s a -> s{_rapPermission = a});

instance FromJSON ResourceAccessPolicy where
        parseJSON
          = withObject "ResourceAccessPolicy"
              (\ x ->
                 ResourceAccessPolicy' <$>
                   (x .:? "ResourceId") <*> (x .:? "Permission"))

instance Hashable ResourceAccessPolicy where

instance NFData ResourceAccessPolicy where

instance ToJSON ResourceAccessPolicy where
        toJSON ResourceAccessPolicy'{..}
          = object
              (catMaybes
                 [("ResourceId" .=) <$> _rapResourceId,
                  ("Permission" .=) <$> _rapPermission])

-- | A container of data for all resource types.
--
-- /See:/ 'resourceDataContainer' smart constructor.
data ResourceDataContainer = ResourceDataContainer'
  { _rdcLocalVolumeResourceData :: !(Maybe LocalVolumeResourceData)
  , _rdcLocalDeviceResourceData :: !(Maybe LocalDeviceResourceData)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceDataContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcLocalVolumeResourceData' - Attributes that define the Local Volume Resource.
--
-- * 'rdcLocalDeviceResourceData' - Attributes that define the Local Device Resource.
resourceDataContainer
    :: ResourceDataContainer
resourceDataContainer =
  ResourceDataContainer'
  {_rdcLocalVolumeResourceData = Nothing, _rdcLocalDeviceResourceData = Nothing}


-- | Attributes that define the Local Volume Resource.
rdcLocalVolumeResourceData :: Lens' ResourceDataContainer (Maybe LocalVolumeResourceData)
rdcLocalVolumeResourceData = lens _rdcLocalVolumeResourceData (\ s a -> s{_rdcLocalVolumeResourceData = a});

-- | Attributes that define the Local Device Resource.
rdcLocalDeviceResourceData :: Lens' ResourceDataContainer (Maybe LocalDeviceResourceData)
rdcLocalDeviceResourceData = lens _rdcLocalDeviceResourceData (\ s a -> s{_rdcLocalDeviceResourceData = a});

instance FromJSON ResourceDataContainer where
        parseJSON
          = withObject "ResourceDataContainer"
              (\ x ->
                 ResourceDataContainer' <$>
                   (x .:? "LocalVolumeResourceData") <*>
                     (x .:? "LocalDeviceResourceData"))

instance Hashable ResourceDataContainer where

instance NFData ResourceDataContainer where

instance ToJSON ResourceDataContainer where
        toJSON ResourceDataContainer'{..}
          = object
              (catMaybes
                 [("LocalVolumeResourceData" .=) <$>
                    _rdcLocalVolumeResourceData,
                  ("LocalDeviceResourceData" .=) <$>
                    _rdcLocalDeviceResourceData])

-- | Information on resource definition version
--
-- /See:/ 'resourceDefinitionVersion' smart constructor.
newtype ResourceDefinitionVersion = ResourceDefinitionVersion'
  { _rdvResources :: Maybe [Resource]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdvResources' - List of resources.
resourceDefinitionVersion
    :: ResourceDefinitionVersion
resourceDefinitionVersion = ResourceDefinitionVersion' {_rdvResources = Nothing}


-- | List of resources.
rdvResources :: Lens' ResourceDefinitionVersion [Resource]
rdvResources = lens _rdvResources (\ s a -> s{_rdvResources = a}) . _Default . _Coerce;

instance FromJSON ResourceDefinitionVersion where
        parseJSON
          = withObject "ResourceDefinitionVersion"
              (\ x ->
                 ResourceDefinitionVersion' <$>
                   (x .:? "Resources" .!= mempty))

instance Hashable ResourceDefinitionVersion where

instance NFData ResourceDefinitionVersion where

instance ToJSON ResourceDefinitionVersion where
        toJSON ResourceDefinitionVersion'{..}
          = object
              (catMaybes [("Resources" .=) <$> _rdvResources])

-- | Information on subscription
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sSubject :: !(Maybe Text)
  , _sSource  :: !(Maybe Text)
  , _sId      :: !(Maybe Text)
  , _sTarget  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubject' - Subject of the message.
--
-- * 'sSource' - Source of the subscription. Can be a thing arn, lambda arn or word 'cloud'
--
-- * 'sId' - Element Id for this entry in the list.
--
-- * 'sTarget' - Where the message is sent to. Can be a thing arn, lambda arn or word 'cloud'.
subscription
    :: Subscription
subscription =
  Subscription'
  {_sSubject = Nothing, _sSource = Nothing, _sId = Nothing, _sTarget = Nothing}


-- | Subject of the message.
sSubject :: Lens' Subscription (Maybe Text)
sSubject = lens _sSubject (\ s a -> s{_sSubject = a});

-- | Source of the subscription. Can be a thing arn, lambda arn or word 'cloud'
sSource :: Lens' Subscription (Maybe Text)
sSource = lens _sSource (\ s a -> s{_sSource = a});

-- | Element Id for this entry in the list.
sId :: Lens' Subscription (Maybe Text)
sId = lens _sId (\ s a -> s{_sId = a});

-- | Where the message is sent to. Can be a thing arn, lambda arn or word 'cloud'.
sTarget :: Lens' Subscription (Maybe Text)
sTarget = lens _sTarget (\ s a -> s{_sTarget = a});

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ x ->
                 Subscription' <$>
                   (x .:? "Subject") <*> (x .:? "Source") <*>
                     (x .:? "Id")
                     <*> (x .:? "Target"))

instance Hashable Subscription where

instance NFData Subscription where

instance ToJSON Subscription where
        toJSON Subscription'{..}
          = object
              (catMaybes
                 [("Subject" .=) <$> _sSubject,
                  ("Source" .=) <$> _sSource, ("Id" .=) <$> _sId,
                  ("Target" .=) <$> _sTarget])

-- | Information on subscription definition version
--
-- /See:/ 'subscriptionDefinitionVersion' smart constructor.
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
  { _sdvSubscriptions :: Maybe [Subscription]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdvSubscriptions' - Subscriptions in the version.
subscriptionDefinitionVersion
    :: SubscriptionDefinitionVersion
subscriptionDefinitionVersion =
  SubscriptionDefinitionVersion' {_sdvSubscriptions = Nothing}


-- | Subscriptions in the version.
sdvSubscriptions :: Lens' SubscriptionDefinitionVersion [Subscription]
sdvSubscriptions = lens _sdvSubscriptions (\ s a -> s{_sdvSubscriptions = a}) . _Default . _Coerce;

instance FromJSON SubscriptionDefinitionVersion where
        parseJSON
          = withObject "SubscriptionDefinitionVersion"
              (\ x ->
                 SubscriptionDefinitionVersion' <$>
                   (x .:? "Subscriptions" .!= mempty))

instance Hashable SubscriptionDefinitionVersion where

instance NFData SubscriptionDefinitionVersion where

instance ToJSON SubscriptionDefinitionVersion where
        toJSON SubscriptionDefinitionVersion'{..}
          = object
              (catMaybes
                 [("Subscriptions" .=) <$> _sdvSubscriptions])

-- | Information on the version
--
-- /See:/ 'versionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { _viARN               :: !(Maybe Text)
  , _viCreationTimestamp :: !(Maybe Text)
  , _viVersion           :: !(Maybe Text)
  , _viId                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VersionInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viARN' - Arn of the version.
--
-- * 'viCreationTimestamp' - Timestamp of when the version was created.
--
-- * 'viVersion' - Unique Id of a version.
--
-- * 'viId' - Id of the resource container.
versionInformation
    :: VersionInformation
versionInformation =
  VersionInformation'
  { _viARN = Nothing
  , _viCreationTimestamp = Nothing
  , _viVersion = Nothing
  , _viId = Nothing
  }


-- | Arn of the version.
viARN :: Lens' VersionInformation (Maybe Text)
viARN = lens _viARN (\ s a -> s{_viARN = a});

-- | Timestamp of when the version was created.
viCreationTimestamp :: Lens' VersionInformation (Maybe Text)
viCreationTimestamp = lens _viCreationTimestamp (\ s a -> s{_viCreationTimestamp = a});

-- | Unique Id of a version.
viVersion :: Lens' VersionInformation (Maybe Text)
viVersion = lens _viVersion (\ s a -> s{_viVersion = a});

-- | Id of the resource container.
viId :: Lens' VersionInformation (Maybe Text)
viId = lens _viId (\ s a -> s{_viId = a});

instance FromJSON VersionInformation where
        parseJSON
          = withObject "VersionInformation"
              (\ x ->
                 VersionInformation' <$>
                   (x .:? "Arn") <*> (x .:? "CreationTimestamp") <*>
                     (x .:? "Version")
                     <*> (x .:? "Id"))

instance Hashable VersionInformation where

instance NFData VersionInformation where
