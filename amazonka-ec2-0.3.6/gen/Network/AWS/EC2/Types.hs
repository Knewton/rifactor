{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.EC2.Types
    (
    -- * Service
      EC2
    -- ** Error
    , EC2Error
    -- ** XML
    , ns

    -- * ImageAttributeName
    , ImageAttributeName (..)

    -- * PermissionGroup
    , PermissionGroup (..)

    -- * NetworkAclEntry
    , NetworkAclEntry
    , networkAclEntry
    , naeCidrBlock
    , naeEgress
    , naeIcmpTypeCode
    , naePortRange
    , naeProtocol
    , naeRuleAction
    , naeRuleNumber

    -- * BlobAttributeValue
    , BlobAttributeValue
    , blobAttributeValue
    , bavValue

    -- * ImportInstanceLaunchSpecification
    , ImportInstanceLaunchSpecification
    , importInstanceLaunchSpecification
    , iilsAdditionalInfo
    , iilsArchitecture
    , iilsGroupIds
    , iilsGroupNames
    , iilsInstanceInitiatedShutdownBehavior
    , iilsInstanceType
    , iilsMonitoring
    , iilsPlacement
    , iilsPrivateIpAddress
    , iilsSubnetId
    , iilsUserData

    -- * Snapshot
    , Snapshot
    , snapshot
    , sDescription
    , sEncrypted
    , sKmsKeyId
    , sOwnerAlias
    , sOwnerId
    , sProgress
    , sSnapshotId
    , sStartTime
    , sState
    , sTags
    , sVolumeId
    , sVolumeSize

    -- * SpotInstanceStateFault
    , SpotInstanceStateFault
    , spotInstanceStateFault
    , sisfCode
    , sisfMessage

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdKey
    , tdResourceId
    , tdResourceType
    , tdValue

    -- * ImportSnapshotTask
    , ImportSnapshotTask
    , importSnapshotTask
    , istDescription
    , istImportTaskId
    , istSnapshotTaskDetail

    -- * GroupIdentifier
    , GroupIdentifier
    , groupIdentifier
    , giGroupId
    , giGroupName

    -- * VpnStaticRouteSource
    , VpnStaticRouteSource (..)

    -- * ReservedInstancesListing
    , ReservedInstancesListing
    , reservedInstancesListing
    , rilClientToken
    , rilCreateDate
    , rilInstanceCounts
    , rilPriceSchedules
    , rilReservedInstancesId
    , rilReservedInstancesListingId
    , rilStatus
    , rilStatusMessage
    , rilTags
    , rilUpdateDate

    -- * InstanceLifecycleType
    , InstanceLifecycleType (..)

    -- * State
    , State (..)

    -- * VirtualizationType
    , VirtualizationType (..)

    -- * NetworkInterfaceStatus
    , NetworkInterfaceStatus (..)

    -- * PlatformValues
    , PlatformValues (..)

    -- * CreateVolumePermission
    , CreateVolumePermission
    , createVolumePermission
    , cvpGroup
    , cvpUserId

    -- * NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges
    , networkInterfaceAttachmentChanges
    , niacAttachmentId
    , niacDeleteOnTermination

    -- * RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- * DhcpOptions
    , DhcpOptions
    , dhcpOptions
    , doDhcpConfigurations
    , doDhcpOptionsId
    , doTags

    -- * InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification
    , instanceNetworkInterfaceSpecification
    , inisAssociatePublicIpAddress
    , inisDeleteOnTermination
    , inisDescription
    , inisDeviceIndex
    , inisGroups
    , inisNetworkInterfaceId
    , inisPrivateIpAddress
    , inisPrivateIpAddresses
    , inisSecondaryPrivateIpAddressCount
    , inisSubnetId

    -- * VolumeState
    , VolumeState (..)

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avValue

    -- * PrivateIpAddressSpecification
    , PrivateIpAddressSpecification
    , privateIpAddressSpecification
    , piasPrimary
    , piasPrivateIpAddress

    -- * Image
    , Image
    , image
    , iArchitecture
    , iBlockDeviceMappings
    , iCreationDate
    , iDescription
    , iHypervisor
    , iImageId
    , iImageLocation
    , iImageOwnerAlias
    , iImageType
    , iKernelId
    , iName
    , iOwnerId
    , iPlatform
    , iProductCodes
    , iPublic
    , iRamdiskId
    , iRootDeviceName
    , iRootDeviceType
    , iSriovNetSupport
    , iState
    , iStateReason
    , iTags
    , iVirtualizationType

    -- * DhcpConfiguration
    , DhcpConfiguration
    , dhcpConfiguration
    , dcKey
    , dcValues

    -- * CancelSpotFleetRequestsError
    , CancelSpotFleetRequestsError
    , cancelSpotFleetRequestsError
    , csfreCode
    , csfreMessage

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * AccountAttributeName
    , AccountAttributeName (..)

    -- * NetworkInterfaceAttachment
    , NetworkInterfaceAttachment
    , networkInterfaceAttachment
    , niaAttachTime
    , niaAttachmentId
    , niaDeleteOnTermination
    , niaDeviceIndex
    , niaInstanceId
    , niaInstanceOwnerId
    , niaStatus

    -- * RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled
    , runInstancesMonitoringEnabled
    , rimeEnabled

    -- * VolumeStatusInfo
    , VolumeStatusInfo
    , volumeStatusInfo
    , vsiDetails
    , vsiStatus

    -- * NetworkInterfaceAssociation
    , NetworkInterfaceAssociation
    , networkInterfaceAssociation
    , niaAllocationId
    , niaAssociationId
    , niaIpOwnerId
    , niaPublicDnsName
    , niaPublicIp

    -- * CreateVolumePermissionModifications
    , CreateVolumePermissionModifications
    , createVolumePermissionModifications
    , cvpmAdd
    , cvpmRemove

    -- * VpcState
    , VpcState (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ReportStatusType
    , ReportStatusType (..)

    -- * CurrencyCodeValues
    , CurrencyCodeValues (..)

    -- * IcmpTypeCode
    , IcmpTypeCode
    , icmpTypeCode
    , itcCode
    , itcType

    -- * InstanceCount
    , InstanceCount
    , instanceCount
    , icInstanceCount
    , icState

    -- * ExportToS3Task
    , ExportToS3Task
    , exportToS3Task
    , etstContainerFormat
    , etstDiskImageFormat
    , etstS3Bucket
    , etstS3Key

    -- * PrefixList
    , PrefixList
    , prefixList
    , plCidrs
    , plPrefixListId
    , plPrefixListName

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmDeviceName
    , bdmEbs
    , bdmNoDevice
    , bdmVirtualName

    -- * ConversionTask
    , ConversionTask
    , conversionTask
    , ctConversionTaskId
    , ctExpirationTime
    , ctImportInstance
    , ctImportVolume
    , ctState
    , ctStatusMessage
    , ctTags

    -- * AttachmentStatus
    , AttachmentStatus (..)

    -- * ClassicLinkInstance
    , ClassicLinkInstance
    , classicLinkInstance
    , cliGroups
    , cliInstanceId
    , cliTags
    , cliVpcId

    -- * RouteOrigin
    , RouteOrigin (..)

    -- * ListingState
    , ListingState (..)

    -- * SpotPrice
    , SpotPrice
    , spotPrice
    , spAvailabilityZone
    , spInstanceType
    , spProductDescription
    , spSpotPrice
    , spTimestamp

    -- * ActiveInstance
    , ActiveInstance
    , activeInstance
    , aiInstanceId
    , aiInstanceType
    , aiSpotInstanceRequestId

    -- * SpotFleetRequestConfigData
    , SpotFleetRequestConfigData
    , spotFleetRequestConfigData
    , sfrcdClientToken
    , sfrcdIamFleetRole
    , sfrcdLaunchSpecifications
    , sfrcdSpotPrice
    , sfrcdTargetCapacity
    , sfrcdTerminateInstancesWithExpiration
    , sfrcdValidFrom
    , sfrcdValidUntil

    -- * InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imInstanceId
    , imMonitoring

    -- * PriceScheduleSpecification
    , PriceScheduleSpecification
    , priceScheduleSpecification
    , pssCurrencyCode
    , pssPrice
    , pssTerm

    -- * SpotFleetRequestConfig
    , SpotFleetRequestConfig
    , spotFleetRequestConfig
    , sfrcSpotFleetRequestConfig
    , sfrcSpotFleetRequestId
    , sfrcSpotFleetRequestState

    -- * SpotInstanceStatus
    , SpotInstanceStatus
    , spotInstanceStatus
    , sisCode
    , sisMessage
    , sisUpdateTime

    -- * SnapshotTaskDetail
    , SnapshotTaskDetail
    , snapshotTaskDetail
    , stdDescription
    , stdDiskImageSize
    , stdFormat
    , stdProgress
    , stdSnapshotId
    , stdStatus
    , stdStatusMessage
    , stdUrl
    , stdUserBucket

    -- * AvailabilityZoneState
    , AvailabilityZoneState (..)

    -- * SpotInstanceRequest
    , SpotInstanceRequest
    , spotInstanceRequest
    , siAvailabilityZoneGroup
    , siCreateTime
    , siFault
    , siInstanceId
    , siLaunchGroup
    , siLaunchSpecification
    , siLaunchedAvailabilityZone
    , siProductDescription
    , siSpotInstanceRequestId
    , siSpotPrice
    , siState
    , siStatus
    , siTags
    , siType
    , siValidFrom
    , siValidUntil

    -- * LaunchSpecification
    , LaunchSpecification
    , launchSpecification
    , lsAddressingType
    , lsBlockDeviceMappings
    , lsEbsOptimized
    , lsIamInstanceProfile
    , lsImageId
    , lsInstanceType
    , lsKernelId
    , lsKeyName
    , lsMonitoring
    , lsNetworkInterfaces
    , lsPlacement
    , lsRamdiskId
    , lsSecurityGroups
    , lsSubnetId
    , lsUserData

    -- * VolumeStatusEvent
    , VolumeStatusEvent
    , volumeStatusEvent
    , vseDescription
    , vseEventId
    , vseEventType
    , vseNotAfter
    , vseNotBefore

    -- * Volume
    , Volume
    , volume
    , vAttachments
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vIops
    , vKmsKeyId
    , vSize
    , vSnapshotId
    , vState
    , vTags
    , vVolumeId
    , vVolumeType

    -- * Reservation
    , Reservation
    , reservation
    , rGroups
    , rInstances
    , rOwnerId
    , rRequesterId
    , rReservationId

    -- * ImportInstanceVolumeDetailItem
    , ImportInstanceVolumeDetailItem
    , importInstanceVolumeDetailItem
    , iivdiAvailabilityZone
    , iivdiBytesConverted
    , iivdiDescription
    , iivdiImage
    , iivdiStatus
    , iivdiStatusMessage
    , iivdiVolume

    -- * SummaryStatus
    , SummaryStatus (..)

    -- * ReservedInstancesModification
    , ReservedInstancesModification
    , reservedInstancesModification
    , rimClientToken
    , rimCreateDate
    , rimEffectiveDate
    , rimModificationResults
    , rimReservedInstancesIds
    , rimReservedInstancesModificationId
    , rimStatus
    , rimStatusMessage
    , rimUpdateDate

    -- * RuleAction
    , RuleAction (..)

    -- * BatchState
    , BatchState (..)

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niAssociation
    , niAttachment
    , niAvailabilityZone
    , niDescription
    , niGroups
    , niMacAddress
    , niNetworkInterfaceId
    , niOwnerId
    , niPrivateDnsName
    , niPrivateIpAddress
    , niPrivateIpAddresses
    , niRequesterId
    , niRequesterManaged
    , niSourceDestCheck
    , niStatus
    , niSubnetId
    , niTagSet
    , niVpcId

    -- * TelemetryStatus
    , TelemetryStatus (..)

    -- * Subnet
    , Subnet
    , subnet
    , s1AvailabilityZone
    , s1AvailableIpAddressCount
    , s1CidrBlock
    , s1DefaultForAz
    , s1MapPublicIpOnLaunch
    , s1State
    , s1SubnetId
    , s1Tags
    , s1VpcId

    -- * KeyPairInfo
    , KeyPairInfo
    , keyPairInfo
    , kpiKeyFingerprint
    , kpiKeyName

    -- * LaunchPermissionModifications
    , LaunchPermissionModifications
    , launchPermissionModifications
    , lpmAdd
    , lpmRemove

    -- * SnapshotState
    , SnapshotState (..)

    -- * InstanceNetworkInterfaceAssociation
    , InstanceNetworkInterfaceAssociation
    , instanceNetworkInterfaceAssociation
    , iniaIpOwnerId
    , iniaPublicDnsName
    , iniaPublicIp

    -- * DiskImageDetail
    , DiskImageDetail
    , diskImageDetail
    , didBytes
    , didFormat
    , didImportManifestUrl

    -- * InstancePrivateIpAddress
    , InstancePrivateIpAddress
    , instancePrivateIpAddress
    , ipiaAssociation
    , ipiaPrimary
    , ipiaPrivateDnsName
    , ipiaPrivateIpAddress

    -- * CancelledSpotInstanceRequest
    , CancelledSpotInstanceRequest
    , cancelledSpotInstanceRequest
    , csiSpotInstanceRequestId
    , csiState

    -- * VpnConnectionOptionsSpecification
    , VpnConnectionOptionsSpecification
    , vpnConnectionOptionsSpecification
    , vcosStaticRoutesOnly

    -- * Address
    , Address
    , address
    , aAllocationId
    , aAssociationId
    , aDomain
    , aInstanceId
    , aNetworkInterfaceId
    , aNetworkInterfaceOwnerId
    , aPrivateIpAddress
    , aPublicIp

    -- * VolumeAttachmentState
    , VolumeAttachmentState (..)

    -- * MovingAddressStatus
    , MovingAddressStatus
    , movingAddressStatus
    , masMoveStatus
    , masPublicIp

    -- * LaunchPermission
    , LaunchPermission
    , launchPermission
    , lpGroup
    , lpUserId

    -- * RouteState
    , RouteState (..)

    -- * RouteTableAssociation
    , RouteTableAssociation
    , routeTableAssociation
    , rtaMain
    , rtaRouteTableAssociationId
    , rtaRouteTableId
    , rtaSubnetId

    -- * BundleTaskState
    , BundleTaskState (..)

    -- * PortRange
    , PortRange
    , portRange
    , prFrom
    , prTo

    -- * VpcAttributeName
    , VpcAttributeName (..)

    -- * ReservedInstancesConfiguration
    , ReservedInstancesConfiguration
    , reservedInstancesConfiguration
    , ricAvailabilityZone
    , ricInstanceCount
    , ricInstanceType
    , ricPlatform

    -- * VolumeStatusDetails
    , VolumeStatusDetails
    , volumeStatusDetails
    , vsdName
    , vsdStatus

    -- * SpotInstanceState
    , SpotInstanceState (..)

    -- * VpnConnectionOptions
    , VpnConnectionOptions
    , vpnConnectionOptions
    , vcoStaticRoutesOnly

    -- * UserIdGroupPair
    , UserIdGroupPair
    , userIdGroupPair
    , uigpGroupId
    , uigpGroupName
    , uigpUserId

    -- * InstanceStatusSummary
    , InstanceStatusSummary
    , instanceStatusSummary
    , issDetails
    , issStatus

    -- * SpotPlacement
    , SpotPlacement
    , spotPlacement
    , sp1AvailabilityZone
    , sp1GroupName

    -- * EbsInstanceBlockDeviceSpecification
    , EbsInstanceBlockDeviceSpecification
    , ebsInstanceBlockDeviceSpecification
    , eibdsDeleteOnTermination
    , eibdsVolumeId

    -- * NetworkAclAssociation
    , NetworkAclAssociation
    , networkAclAssociation
    , naaNetworkAclAssociationId
    , naaNetworkAclId
    , naaSubnetId

    -- * BundleTask
    , BundleTask
    , bundleTask
    , btBundleId
    , btBundleTaskError
    , btInstanceId
    , btProgress
    , btStartTime
    , btState
    , btStorage
    , btUpdateTime

    -- * InstanceStatusEvent
    , InstanceStatusEvent
    , instanceStatusEvent
    , iseCode
    , iseDescription
    , iseNotAfter
    , iseNotBefore

    -- * InstanceType
    , InstanceType (..)

    -- * Route
    , Route
    , route
    , rDestinationCidrBlock
    , rDestinationPrefixListId
    , rGatewayId
    , rInstanceId
    , rInstanceOwnerId
    , rNetworkInterfaceId
    , rOrigin
    , rState
    , rVpcPeeringConnectionId

    -- * SpotDatafeedSubscription
    , SpotDatafeedSubscription
    , spotDatafeedSubscription
    , sdsBucket
    , sdsFault
    , sdsOwnerId
    , sdsPrefix
    , sdsState

    -- * Storage
    , Storage
    , storage
    , sS3

    -- * SecurityGroup
    , SecurityGroup
    , securityGroup
    , sgDescription
    , sgGroupId
    , sgGroupName
    , sgIpPermissions
    , sgIpPermissionsEgress
    , sgOwnerId
    , sgTags
    , sgVpcId

    -- * CancelSpotInstanceRequestState
    , CancelSpotInstanceRequestState (..)

    -- * PlacementGroupState
    , PlacementGroupState (..)

    -- * ReservedInstancesModificationResult
    , ReservedInstancesModificationResult
    , reservedInstancesModificationResult
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- * InstanceBlockDeviceMappingSpecification
    , InstanceBlockDeviceMappingSpecification
    , instanceBlockDeviceMappingSpecification
    , ibdmsDeviceName
    , ibdmsEbs
    , ibdmsNoDevice
    , ibdmsVirtualName

    -- * ExportEnvironment
    , ExportEnvironment (..)

    -- * UserData
    , UserData
    , userData
    , udData

    -- * VolumeAttachment
    , VolumeAttachment
    , volumeAttachment
    , vaAttachTime
    , vaDeleteOnTermination
    , vaDevice
    , vaInstanceId
    , vaState
    , vaVolumeId

    -- * CustomerGateway
    , CustomerGateway
    , customerGateway
    , cgBgpAsn
    , cgCustomerGatewayId
    , cgIpAddress
    , cgState
    , cgTags
    , cgType

    -- * EbsInstanceBlockDevice
    , EbsInstanceBlockDevice
    , ebsInstanceBlockDevice
    , eibdAttachTime
    , eibdDeleteOnTermination
    , eibdStatus
    , eibdVolumeId

    -- * ShutdownBehavior
    , ShutdownBehavior (..)

    -- * DiskImageDescription
    , DiskImageDescription
    , diskImageDescription
    , did1Checksum
    , did1Format
    , did1ImportManifestUrl
    , did1Size

    -- * DiskImageVolumeDescription
    , DiskImageVolumeDescription
    , diskImageVolumeDescription
    , divdId
    , divdSize

    -- * Monitoring
    , Monitoring
    , monitoring
    , mState

    -- * SubnetState
    , SubnetState (..)

    -- * CancelSpotFleetRequestsSuccessItem
    , CancelSpotFleetRequestsSuccessItem
    , cancelSpotFleetRequestsSuccessItem
    , csfrsiCurrentSpotFleetRequestState
    , csfrsiPreviousSpotFleetRequestState
    , csfrsiSpotFleetRequestId

    -- * ContainerFormat
    , ContainerFormat (..)

    -- * AvailabilityZoneMessage
    , AvailabilityZoneMessage
    , availabilityZoneMessage
    , azmMessage

    -- * VpcAttachment
    , VpcAttachment
    , vpcAttachment
    , va1State
    , va1VpcId

    -- * EventType
    , EventType (..)

    -- * InstanceBlockDeviceMapping
    , InstanceBlockDeviceMapping
    , instanceBlockDeviceMapping
    , ibdmDeviceName
    , ibdmEbs

    -- * StatusType
    , StatusType (..)

    -- * ExportToS3TaskSpecification
    , ExportToS3TaskSpecification
    , exportToS3TaskSpecification
    , etstsContainerFormat
    , etstsDiskImageFormat
    , etstsS3Bucket
    , etstsS3Prefix

    -- * CancelBatchErrorCode
    , CancelBatchErrorCode (..)

    -- * PrefixListId
    , PrefixListId
    , prefixListId
    , pliPrefixListId

    -- * NetworkInterfaceAttribute
    , NetworkInterfaceAttribute (..)

    -- * ImageTypeValues
    , ImageTypeValues (..)

    -- * InstanceExportDetails
    , InstanceExportDetails
    , instanceExportDetails
    , iedInstanceId
    , iedTargetEnvironment

    -- * SnapshotAttributeName
    , SnapshotAttributeName (..)

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azMessages
    , azRegionName
    , azState
    , azZoneName

    -- * HistoryRecord
    , HistoryRecord
    , historyRecord
    , hrEventInformation
    , hrEventType
    , hrTimestamp

    -- * ImportImageTask
    , ImportImageTask
    , importImageTask
    , iitArchitecture
    , iitDescription
    , iitHypervisor
    , iitImageId
    , iitImportTaskId
    , iitLicenseType
    , iitPlatform
    , iitProgress
    , iitSnapshotDetails
    , iitStatus
    , iitStatusMessage

    -- * VpnState
    , VpnState (..)

    -- * RouteTable
    , RouteTable
    , routeTable
    , rtAssociations
    , rtPropagatingVgws
    , rtRouteTableId
    , rtRoutes
    , rtTags
    , rtVpcId

    -- * UserBucket
    , UserBucket
    , userBucket
    , ubS3Bucket
    , ubS3Key

    -- * HypervisorType
    , HypervisorType (..)

    -- * CancelSpotFleetRequestsErrorItem
    , CancelSpotFleetRequestsErrorItem
    , cancelSpotFleetRequestsErrorItem
    , csfreiError
    , csfreiSpotFleetRequestId

    -- * InstanceStatusDetails
    , InstanceStatusDetails
    , instanceStatusDetails
    , isdImpairedSince
    , isdName
    , isdStatus

    -- * IamInstanceProfile
    , IamInstanceProfile
    , iamInstanceProfile
    , iipArn
    , iipId

    -- * UnsuccessfulItem
    , UnsuccessfulItem
    , unsuccessfulItem
    , uiError
    , uiResourceId

    -- * InternetGatewayAttachment
    , InternetGatewayAttachment
    , internetGatewayAttachment
    , igaState
    , igaVpcId

    -- * AddressStatus
    , AddressStatus (..)

    -- * ReservedInstanceState
    , ReservedInstanceState (..)

    -- * InstanceAttributeName
    , InstanceAttributeName (..)

    -- * IpPermission
    , IpPermission
    , ipPermission
    , ipFromPort
    , ipIpProtocol
    , ipIpRanges
    , ipPrefixListIds
    , ipToPort
    , ipUserIdGroupPairs

    -- * ConversionTaskState
    , ConversionTaskState (..)

    -- * DiskImage
    , DiskImage
    , diskImage
    , diDescription
    , diImage
    , diVolume

    -- * Tenancy
    , Tenancy (..)

    -- * UnsuccessfulItemError
    , UnsuccessfulItemError
    , unsuccessfulItemError
    , uieCode
    , uieMessage

    -- * VpcPeeringConnectionStateReason
    , VpcPeeringConnectionStateReason
    , vpcPeeringConnectionStateReason
    , vpcsrCode
    , vpcsrMessage

    -- * IamInstanceProfileSpecification
    , IamInstanceProfileSpecification
    , iamInstanceProfileSpecification
    , iipsArn
    , iipsName

    -- * ImportVolumeTaskDetails
    , ImportVolumeTaskDetails
    , importVolumeTaskDetails
    , ivtdAvailabilityZone
    , ivtdBytesConverted
    , ivtdDescription
    , ivtdImage
    , ivtdVolume

    -- * PlacementStrategy
    , PlacementStrategy (..)

    -- * InstanceNetworkInterface
    , InstanceNetworkInterface
    , instanceNetworkInterface
    , iniAssociation
    , iniAttachment
    , iniDescription
    , iniGroups
    , iniMacAddress
    , iniNetworkInterfaceId
    , iniOwnerId
    , iniPrivateDnsName
    , iniPrivateIpAddress
    , iniPrivateIpAddresses
    , iniSourceDestCheck
    , iniStatus
    , iniSubnetId
    , iniVpcId

    -- * VolumeStatusAction
    , VolumeStatusAction
    , volumeStatusAction
    , vsaCode
    , vsaDescription
    , vsaEventId
    , vsaEventType

    -- * VpcPeeringConnectionVpcInfo
    , VpcPeeringConnectionVpcInfo
    , vpcPeeringConnectionVpcInfo
    , vpcviCidrBlock
    , vpcviOwnerId
    , vpcviVpcId

    -- * UserBucketDetails
    , UserBucketDetails
    , userBucketDetails
    , ubdS3Bucket
    , ubdS3Key

    -- * ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice
    , reservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- * Vpc
    , Vpc
    , vpc
    , vpcCidrBlock
    , vpcDhcpOptionsId
    , vpcInstanceTenancy
    , vpcIsDefault
    , vpcState
    , vpcTags
    , vpcVpcId

    -- * ImageDiskContainer
    , ImageDiskContainer
    , imageDiskContainer
    , idcDescription
    , idcDeviceName
    , idcFormat
    , idcSnapshotId
    , idcUrl
    , idcUserBucket

    -- * InstanceStatus
    , InstanceStatus
    , instanceStatus
    , isAvailabilityZone
    , isEvents
    , isInstanceId
    , isInstanceState
    , isInstanceStatus
    , isSystemStatus

    -- * ArchitectureValues
    , ArchitectureValues (..)

    -- * ReportInstanceReasonCodes
    , ReportInstanceReasonCodes (..)

    -- * MoveStatus
    , MoveStatus (..)

    -- * EbsBlockDevice
    , EbsBlockDevice
    , ebsBlockDevice
    , ebdDeleteOnTermination
    , ebdEncrypted
    , ebdIops
    , ebdSnapshotId
    , ebdVolumeSize
    , ebdVolumeType

    -- * AccountAttribute
    , AccountAttribute
    , accountAttribute
    , aaAttributeName
    , aaAttributeValues

    -- * SnapshotDetail
    , SnapshotDetail
    , snapshotDetail
    , sdDescription
    , sdDeviceName
    , sdDiskImageSize
    , sdFormat
    , sdProgress
    , sdSnapshotId
    , sdStatus
    , sdStatusMessage
    , sdUrl
    , sdUserBucket

    -- * PriceSchedule
    , PriceSchedule
    , priceSchedule
    , psActive
    , psCurrencyCode
    , psPrice
    , psTerm

    -- * DeviceType
    , DeviceType (..)

    -- * DomainType
    , DomainType (..)

    -- * Region
    , Region
    , region
    , rEndpoint
    , rRegionName

    -- * PropagatingVgw
    , PropagatingVgw
    , propagatingVgw
    , pvGatewayId

    -- * OfferingTypeValues
    , OfferingTypeValues (..)

    -- * VpnGateway
    , VpnGateway
    , vpnGateway
    , vgAvailabilityZone
    , vgState
    , vgTags
    , vgType
    , vgVpcAttachments
    , vgVpnGatewayId

    -- * EventInformation
    , EventInformation
    , eventInformation
    , eiEventDescription
    , eiEventSubType
    , eiInstanceId

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * VolumeType
    , VolumeType (..)

    -- * InstanceStateChange
    , InstanceStateChange
    , instanceStateChange
    , iscCurrentState
    , iscInstanceId
    , iscPreviousState

    -- * NetworkAcl
    , NetworkAcl
    , networkAcl
    , naAssociations
    , naEntries
    , naIsDefault
    , naNetworkAclId
    , naTags
    , naVpcId

    -- * ImageState
    , ImageState (..)

    -- * GatewayType
    , GatewayType (..)

    -- * InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment
    , instanceNetworkInterfaceAttachment
    , iniaAttachTime
    , iniaAttachmentId
    , iniaDeleteOnTermination
    , iniaDeviceIndex
    , iniaStatus

    -- * AttributeBooleanValue
    , AttributeBooleanValue
    , attributeBooleanValue
    , abvValue

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcAmount
    , rcFrequency

    -- * NewDhcpConfiguration
    , NewDhcpConfiguration
    , newDhcpConfiguration
    , ndcKey
    , ndcValues

    -- * StateReason
    , StateReason
    , stateReason
    , srCode
    , srMessage

    -- * MonitoringState
    , MonitoringState (..)

    -- * ReservedInstancesId
    , ReservedInstancesId
    , reservedInstancesId
    , riiReservedInstancesId

    -- * StatusName
    , StatusName (..)

    -- * InternetGateway
    , InternetGateway
    , internetGateway
    , igAttachments
    , igInternetGatewayId
    , igTags

    -- * VolumeStatusName
    , VolumeStatusName (..)

    -- * VolumeAttributeName
    , VolumeAttributeName (..)

    -- * ImportInstanceTaskDetails
    , ImportInstanceTaskDetails
    , importInstanceTaskDetails
    , iitdDescription
    , iitdInstanceId
    , iitdPlatform
    , iitdVolumes

    -- * PlacementGroup
    , PlacementGroup
    , placementGroup
    , pgGroupName
    , pgState
    , pgStrategy

    -- * ProductCode
    , ProductCode
    , productCode
    , pcProductCodeId
    , pcProductCodeType

    -- * ListingStatus
    , ListingStatus (..)

    -- * IpRange
    , IpRange
    , ipRange
    , irCidrIp

    -- * VolumeStatusInfoStatus
    , VolumeStatusInfoStatus (..)

    -- * AccountAttributeValue
    , AccountAttributeValue
    , accountAttributeValue
    , aavAttributeValue

    -- * SnapshotDiskContainer
    , SnapshotDiskContainer
    , snapshotDiskContainer
    , sdcDescription
    , sdcFormat
    , sdcUrl
    , sdcUserBucket

    -- * RIProductDescription
    , RIProductDescription (..)

    -- * ReservedInstancesOffering
    , ReservedInstancesOffering
    , reservedInstancesOffering
    , rioAvailabilityZone
    , rioCurrencyCode
    , rioDuration
    , rioFixedPrice
    , rioInstanceTenancy
    , rioInstanceType
    , rioMarketplace
    , rioOfferingType
    , rioPricingDetails
    , rioProductDescription
    , rioRecurringCharges
    , rioReservedInstancesOfferingId
    , rioUsagePrice

    -- * ReservedInstances
    , ReservedInstances
    , reservedInstances
    , ri1AvailabilityZone
    , ri1CurrencyCode
    , ri1Duration
    , ri1End
    , ri1FixedPrice
    , ri1InstanceCount
    , ri1InstanceTenancy
    , ri1InstanceType
    , ri1OfferingType
    , ri1ProductDescription
    , ri1RecurringCharges
    , ri1ReservedInstancesId
    , ri1Start
    , ri1State
    , ri1Tags
    , ri1UsagePrice

    -- * DatafeedSubscriptionState
    , DatafeedSubscriptionState (..)

    -- * ExportTaskState
    , ExportTaskState (..)

    -- * ProductCodeValues
    , ProductCodeValues (..)

    -- * VpnConnection
    , VpnConnection
    , vpnConnection
    , vcCustomerGatewayConfiguration
    , vcCustomerGatewayId
    , vcOptions
    , vcRoutes
    , vcState
    , vcTags
    , vcType
    , vcVgwTelemetry
    , vcVpnConnectionId
    , vcVpnGatewayId

    -- * InstanceState
    , InstanceState
    , instanceState
    , isCode
    , isName

    -- * VpcEndpoint
    , VpcEndpoint
    , vpcEndpoint
    , veCreationTimestamp
    , vePolicyDocument
    , veRouteTableIds
    , veServiceName
    , veState
    , veVpcEndpointId
    , veVpcId

    -- * ClientData
    , ClientData
    , clientData
    , cdComment
    , cdUploadEnd
    , cdUploadSize
    , cdUploadStart

    -- * Placement
    , Placement
    , placement
    , pAvailabilityZone
    , pGroupName
    , pTenancy

    -- * EventCode
    , EventCode (..)

    -- * SpotInstanceType
    , SpotInstanceType (..)

    -- * VpcPeeringConnection
    , VpcPeeringConnection
    , vpcPeeringConnection
    , vpc1AccepterVpcInfo
    , vpc1ExpirationTime
    , vpc1RequesterVpcInfo
    , vpc1Status
    , vpc1Tags
    , vpc1VpcPeeringConnectionId

    -- * S3Storage
    , S3Storage
    , s3Storage
    , ssAWSAccessKeyId
    , ssBucket
    , ssPrefix
    , ssUploadPolicy
    , ssUploadPolicySignature

    -- * VgwTelemetry
    , VgwTelemetry
    , vgwTelemetry
    , vtAcceptedRouteCount
    , vtLastStatusChange
    , vtOutsideIpAddress
    , vtStatus
    , vtStatusMessage

    -- * VpnStaticRoute
    , VpnStaticRoute
    , vpnStaticRoute
    , vsrDestinationCidrBlock
    , vsrSource
    , vsrState

    -- * InstanceStateName
    , InstanceStateName (..)

    -- * Instance
    , Instance
    , instance'
    , i1AmiLaunchIndex
    , i1Architecture
    , i1BlockDeviceMappings
    , i1ClientToken
    , i1EbsOptimized
    , i1Hypervisor
    , i1IamInstanceProfile
    , i1ImageId
    , i1InstanceId
    , i1InstanceLifecycle
    , i1InstanceType
    , i1KernelId
    , i1KeyName
    , i1LaunchTime
    , i1Monitoring
    , i1NetworkInterfaces
    , i1Placement
    , i1Platform
    , i1PrivateDnsName
    , i1PrivateIpAddress
    , i1ProductCodes
    , i1PublicDnsName
    , i1PublicIpAddress
    , i1RamdiskId
    , i1RootDeviceName
    , i1RootDeviceType
    , i1SecurityGroups
    , i1SourceDestCheck
    , i1SpotInstanceRequestId
    , i1SriovNetSupport
    , i1State
    , i1StateReason
    , i1StateTransitionReason
    , i1SubnetId
    , i1Tags
    , i1VirtualizationType
    , i1VpcId

    -- * ExportTask
    , ExportTask
    , exportTask
    , etDescription
    , etExportTaskId
    , etExportToS3Task
    , etInstanceExportDetails
    , etState
    , etStatusMessage

    -- * ResetImageAttributeName
    , ResetImageAttributeName (..)

    -- * RequestSpotLaunchSpecification
    , RequestSpotLaunchSpecification
    , requestSpotLaunchSpecification
    , rslsAddressingType
    , rslsBlockDeviceMappings
    , rslsEbsOptimized
    , rslsIamInstanceProfile
    , rslsImageId
    , rslsInstanceType
    , rslsKernelId
    , rslsKeyName
    , rslsMonitoring
    , rslsNetworkInterfaces
    , rslsPlacement
    , rslsRamdiskId
    , rslsSecurityGroupIds
    , rslsSecurityGroups
    , rslsSubnetId
    , rslsUserData

    -- * VolumeDetail
    , VolumeDetail
    , volumeDetail
    , vdSize

    -- * PricingDetail
    , PricingDetail
    , pricingDetail
    , pdCount
    , pdPrice

    -- * NetworkInterfacePrivateIpAddress
    , NetworkInterfacePrivateIpAddress
    , networkInterfacePrivateIpAddress
    , nipiaAssociation
    , nipiaPrimary
    , nipiaPrivateDnsName
    , nipiaPrivateIpAddress

    -- * DiskImageFormat
    , DiskImageFormat (..)

    -- * BundleTaskError
    , BundleTaskError
    , bundleTaskError
    , bteCode
    , bteMessage

    -- * VpcClassicLink
    , VpcClassicLink
    , vpcClassicLink
    , vclClassicLinkEnabled
    , vclTags
    , vclVpcId

    -- * VolumeStatusItem
    , VolumeStatusItem
    , volumeStatusItem
    , vsiActions
    , vsiAvailabilityZone
    , vsiEvents
    , vsiVolumeId
    , vsiVolumeStatus

    -- * Common
    , module Network.AWS.EC2.Internal
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import Network.AWS.EC2.Internal
import qualified GHC.Exts

-- | Version @2015-04-15@ of the Amazon Elastic Compute Cloud service.
data EC2

instance AWSService EC2 where
    type Sg EC2 = V4
    type Er EC2 = EC2Error

    service = service'
      where
        service' :: Service EC2
        service' = Service
            { _svcAbbrev       = "EC2"
            , _svcPrefix       = "ec2"
            , _svcVersion      = "2015-04-15"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError EC2Error)
        handle = restError statusSuccess service'

        retry :: Retry EC2
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> EC2Error
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 503 && (Just "RequestLimitExceeded") == e = True -- Request Limit Exceeded
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://ec2.amazonaws.com/doc/2015-04-15"
{-# INLINE ns #-}

data ImageAttributeName
    = ImageBlockDeviceMapping -- ^ blockDeviceMapping
    | ImageDescription        -- ^ description
    | ImageKernel             -- ^ kernel
    | ImageLaunchPermission   -- ^ launchPermission
    | ImageProductCodes       -- ^ productCodes
    | ImageRamdisk            -- ^ ramdisk
    | ImageSriovNetSupport    -- ^ sriovNetSupport
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ImageAttributeName

instance FromText ImageAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping" -> pure ImageBlockDeviceMapping
        "description"        -> pure ImageDescription
        "kernel"             -> pure ImageKernel
        "launchpermission"   -> pure ImageLaunchPermission
        "productcodes"       -> pure ImageProductCodes
        "ramdisk"            -> pure ImageRamdisk
        "sriovnetsupport"    -> pure ImageSriovNetSupport
        e                    -> fail $
            "Failure parsing ImageAttributeName from " ++ show e

instance ToText ImageAttributeName where
    toText = \case
        ImageBlockDeviceMapping -> "blockDeviceMapping"
        ImageDescription        -> "description"
        ImageKernel             -> "kernel"
        ImageLaunchPermission   -> "launchPermission"
        ImageProductCodes       -> "productCodes"
        ImageRamdisk            -> "ramdisk"
        ImageSriovNetSupport    -> "sriovNetSupport"

instance ToByteString ImageAttributeName
instance ToHeader     ImageAttributeName
instance ToQuery      ImageAttributeName

instance FromXML ImageAttributeName where
    parseXML = parseXMLText "ImageAttributeName"

data PermissionGroup
    = All -- ^ all
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable PermissionGroup

instance FromText PermissionGroup where
    parser = takeLowerText >>= \case
        "all" -> pure All
        e     -> fail $
            "Failure parsing PermissionGroup from " ++ show e

instance ToText PermissionGroup where
    toText All = "all"

instance ToByteString PermissionGroup
instance ToHeader     PermissionGroup
instance ToQuery      PermissionGroup

instance FromXML PermissionGroup where
    parseXML = parseXMLText "PermissionGroup"

data NetworkAclEntry = NetworkAclEntry
    { _naeCidrBlock    :: Maybe Text
    , _naeEgress       :: Maybe Bool
    , _naeIcmpTypeCode :: Maybe IcmpTypeCode
    , _naePortRange    :: Maybe PortRange
    , _naeProtocol     :: Maybe Text
    , _naeRuleAction   :: Maybe RuleAction
    , _naeRuleNumber   :: Maybe Int
    } deriving (Eq, Read, Show)

-- | 'NetworkAclEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naeCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'naeEgress' @::@ 'Maybe' 'Bool'
--
-- * 'naeIcmpTypeCode' @::@ 'Maybe' 'IcmpTypeCode'
--
-- * 'naePortRange' @::@ 'Maybe' 'PortRange'
--
-- * 'naeProtocol' @::@ 'Maybe' 'Text'
--
-- * 'naeRuleAction' @::@ 'Maybe' 'RuleAction'
--
-- * 'naeRuleNumber' @::@ 'Maybe' 'Int'
--
networkAclEntry :: NetworkAclEntry
networkAclEntry = NetworkAclEntry
    { _naeRuleNumber   = Nothing
    , _naeProtocol     = Nothing
    , _naeRuleAction   = Nothing
    , _naeEgress       = Nothing
    , _naeCidrBlock    = Nothing
    , _naeIcmpTypeCode = Nothing
    , _naePortRange    = Nothing
    }

-- | The network range to allow or deny, in CIDR notation.
naeCidrBlock :: Lens' NetworkAclEntry (Maybe Text)
naeCidrBlock = lens _naeCidrBlock (\s a -> s { _naeCidrBlock = a })

-- | Indicates whether the rule is an egress rule (applied to traffic leaving the
-- subnet).
naeEgress :: Lens' NetworkAclEntry (Maybe Bool)
naeEgress = lens _naeEgress (\s a -> s { _naeEgress = a })

-- | ICMP protocol: The ICMP type and code.
naeIcmpTypeCode :: Lens' NetworkAclEntry (Maybe IcmpTypeCode)
naeIcmpTypeCode = lens _naeIcmpTypeCode (\s a -> s { _naeIcmpTypeCode = a })

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkAclEntry (Maybe PortRange)
naePortRange = lens _naePortRange (\s a -> s { _naePortRange = a })

-- | The protocol. A value of '-1' means all protocols.
naeProtocol :: Lens' NetworkAclEntry (Maybe Text)
naeProtocol = lens _naeProtocol (\s a -> s { _naeProtocol = a })

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkAclEntry (Maybe RuleAction)
naeRuleAction = lens _naeRuleAction (\s a -> s { _naeRuleAction = a })

-- | The rule number for the entry. ACL entries are processed in ascending order
-- by rule number.
naeRuleNumber :: Lens' NetworkAclEntry (Maybe Int)
naeRuleNumber = lens _naeRuleNumber (\s a -> s { _naeRuleNumber = a })

instance FromXML NetworkAclEntry where
    parseXML x = NetworkAclEntry
        <$> x .@? "cidrBlock"
        <*> x .@? "egress"
        <*> x .@? "icmpTypeCode"
        <*> x .@? "portRange"
        <*> x .@? "protocol"
        <*> x .@? "ruleAction"
        <*> x .@? "ruleNumber"

instance ToQuery NetworkAclEntry where
    toQuery NetworkAclEntry{..} = mconcat
        [ "CidrBlock"    =? _naeCidrBlock
        , "Egress"       =? _naeEgress
        , "IcmpTypeCode" =? _naeIcmpTypeCode
        , "PortRange"    =? _naePortRange
        , "Protocol"     =? _naeProtocol
        , "RuleAction"   =? _naeRuleAction
        , "RuleNumber"   =? _naeRuleNumber
        ]

newtype BlobAttributeValue = BlobAttributeValue
    { _bavValue :: Maybe Base64
    } deriving (Eq, Read, Show)

-- | 'BlobAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bavValue' @::@ 'Maybe' 'Base64'
--
blobAttributeValue :: BlobAttributeValue
blobAttributeValue = BlobAttributeValue
    { _bavValue = Nothing
    }

bavValue :: Lens' BlobAttributeValue (Maybe Base64)
bavValue = lens _bavValue (\s a -> s { _bavValue = a })

instance FromXML BlobAttributeValue where
    parseXML x = BlobAttributeValue
        <$> x .@? "value"

instance ToQuery BlobAttributeValue where
    toQuery BlobAttributeValue{..} = mconcat
        [ "Value" =? _bavValue
        ]

data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsAdditionalInfo                    :: Maybe Text
    , _iilsArchitecture                      :: Maybe ArchitectureValues
    , _iilsGroupIds                          :: List "SecurityGroupId" Text
    , _iilsGroupNames                        :: List "SecurityGroup" Text
    , _iilsInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
    , _iilsInstanceType                      :: Maybe InstanceType
    , _iilsMonitoring                        :: Maybe Bool
    , _iilsPlacement                         :: Maybe Placement
    , _iilsPrivateIpAddress                  :: Maybe Text
    , _iilsSubnetId                          :: Maybe Text
    , _iilsUserData                          :: Maybe UserData
    } deriving (Eq, Read, Show)

-- | 'ImportInstanceLaunchSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iilsAdditionalInfo' @::@ 'Maybe' 'Text'
--
-- * 'iilsArchitecture' @::@ 'Maybe' 'ArchitectureValues'
--
-- * 'iilsGroupIds' @::@ ['Text']
--
-- * 'iilsGroupNames' @::@ ['Text']
--
-- * 'iilsInstanceInitiatedShutdownBehavior' @::@ 'Maybe' 'ShutdownBehavior'
--
-- * 'iilsInstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'iilsMonitoring' @::@ 'Maybe' 'Bool'
--
-- * 'iilsPlacement' @::@ 'Maybe' 'Placement'
--
-- * 'iilsPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'iilsSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'iilsUserData' @::@ 'Maybe' 'UserData'
--
importInstanceLaunchSpecification :: ImportInstanceLaunchSpecification
importInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { _iilsArchitecture                      = Nothing
    , _iilsGroupNames                        = mempty
    , _iilsGroupIds                          = mempty
    , _iilsAdditionalInfo                    = Nothing
    , _iilsUserData                          = Nothing
    , _iilsInstanceType                      = Nothing
    , _iilsPlacement                         = Nothing
    , _iilsMonitoring                        = Nothing
    , _iilsSubnetId                          = Nothing
    , _iilsInstanceInitiatedShutdownBehavior = Nothing
    , _iilsPrivateIpAddress                  = Nothing
    }

-- | Reserved.
iilsAdditionalInfo :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsAdditionalInfo =
    lens _iilsAdditionalInfo (\s a -> s { _iilsAdditionalInfo = a })

-- | The architecture of the instance.
iilsArchitecture :: Lens' ImportInstanceLaunchSpecification (Maybe ArchitectureValues)
iilsArchitecture = lens _iilsArchitecture (\s a -> s { _iilsArchitecture = a })

-- | One or more security group IDs.
iilsGroupIds :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupIds = lens _iilsGroupIds (\s a -> s { _iilsGroupIds = a }) . _List

-- | One or more security group names.
iilsGroupNames :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupNames = lens _iilsGroupNames (\s a -> s { _iilsGroupNames = a }) . _List

-- | Indicates whether an instance stops or terminates when you initiate shutdown
-- from the instance (using the operating system command for system shutdown).
iilsInstanceInitiatedShutdownBehavior :: Lens' ImportInstanceLaunchSpecification (Maybe ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior =
    lens _iilsInstanceInitiatedShutdownBehavior
        (\s a -> s { _iilsInstanceInitiatedShutdownBehavior = a })

-- | The instance type. For more information about the instance types that you can
-- import, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/VMImportPrerequisites.html Before You Get Started> in the Amazon Elastic Compute Cloud User
-- Guide.
iilsInstanceType :: Lens' ImportInstanceLaunchSpecification (Maybe InstanceType)
iilsInstanceType = lens _iilsInstanceType (\s a -> s { _iilsInstanceType = a })

-- | Indicates whether monitoring is enabled.
iilsMonitoring :: Lens' ImportInstanceLaunchSpecification (Maybe Bool)
iilsMonitoring = lens _iilsMonitoring (\s a -> s { _iilsMonitoring = a })

-- | The placement information for the instance.
iilsPlacement :: Lens' ImportInstanceLaunchSpecification (Maybe Placement)
iilsPlacement = lens _iilsPlacement (\s a -> s { _iilsPlacement = a })

-- | [EC2-VPC] An available IP address from the IP address range of the subnet.
iilsPrivateIpAddress :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsPrivateIpAddress =
    lens _iilsPrivateIpAddress (\s a -> s { _iilsPrivateIpAddress = a })

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
iilsSubnetId :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsSubnetId = lens _iilsSubnetId (\s a -> s { _iilsSubnetId = a })

-- | The Base64-encoded MIME user data to be made available to the instance.
iilsUserData :: Lens' ImportInstanceLaunchSpecification (Maybe UserData)
iilsUserData = lens _iilsUserData (\s a -> s { _iilsUserData = a })

instance FromXML ImportInstanceLaunchSpecification where
    parseXML x = ImportInstanceLaunchSpecification
        <$> x .@? "additionalInfo"
        <*> x .@? "architecture"
        <*> x .@? "GroupId" .!@ mempty
        <*> x .@? "GroupName" .!@ mempty
        <*> x .@? "instanceInitiatedShutdownBehavior"
        <*> x .@? "instanceType"
        <*> x .@? "monitoring"
        <*> x .@? "placement"
        <*> x .@? "privateIpAddress"
        <*> x .@? "subnetId"
        <*> x .@? "userData"

instance ToQuery ImportInstanceLaunchSpecification where
    toQuery ImportInstanceLaunchSpecification{..} = mconcat
        [ "AdditionalInfo"                    =? _iilsAdditionalInfo
        , "Architecture"                      =? _iilsArchitecture
        , "GroupId"                           `toQueryList` _iilsGroupIds
        , "GroupName"                         `toQueryList` _iilsGroupNames
        , "InstanceInitiatedShutdownBehavior" =? _iilsInstanceInitiatedShutdownBehavior
        , "InstanceType"                      =? _iilsInstanceType
        , "Monitoring"                        =? _iilsMonitoring
        , "Placement"                         =? _iilsPlacement
        , "PrivateIpAddress"                  =? _iilsPrivateIpAddress
        , "SubnetId"                          =? _iilsSubnetId
        , "UserData"                          =? _iilsUserData
        ]

data Snapshot = Snapshot
    { _sDescription :: Text
    , _sEncrypted   :: Bool
    , _sKmsKeyId    :: Maybe Text
    , _sOwnerAlias  :: Maybe Text
    , _sOwnerId     :: Text
    , _sProgress    :: Text
    , _sSnapshotId  :: Text
    , _sStartTime   :: ISO8601
    , _sState       :: SnapshotState
    , _sTags        :: List "item" Tag
    , _sVolumeId    :: Text
    , _sVolumeSize  :: Int
    } deriving (Eq, Read, Show)

-- | 'Snapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sDescription' @::@ 'Text'
--
-- * 'sEncrypted' @::@ 'Bool'
--
-- * 'sKmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'sOwnerAlias' @::@ 'Maybe' 'Text'
--
-- * 'sOwnerId' @::@ 'Text'
--
-- * 'sProgress' @::@ 'Text'
--
-- * 'sSnapshotId' @::@ 'Text'
--
-- * 'sStartTime' @::@ 'UTCTime'
--
-- * 'sState' @::@ 'SnapshotState'
--
-- * 'sTags' @::@ ['Tag']
--
-- * 'sVolumeId' @::@ 'Text'
--
-- * 'sVolumeSize' @::@ 'Int'
--
snapshot :: Text -- ^ 'sSnapshotId'
         -> Text -- ^ 'sVolumeId'
         -> SnapshotState -- ^ 'sState'
         -> UTCTime -- ^ 'sStartTime'
         -> Text -- ^ 'sProgress'
         -> Text -- ^ 'sOwnerId'
         -> Text -- ^ 'sDescription'
         -> Int -- ^ 'sVolumeSize'
         -> Bool -- ^ 'sEncrypted'
         -> Snapshot
snapshot p1 p2 p3 p4 p5 p6 p7 p8 p9 = Snapshot
    { _sSnapshotId  = p1
    , _sVolumeId    = p2
    , _sState       = p3
    , _sStartTime   = withIso _Time (const id) p4
    , _sProgress    = p5
    , _sOwnerId     = p6
    , _sDescription = p7
    , _sVolumeSize  = p8
    , _sEncrypted   = p9
    , _sOwnerAlias  = Nothing
    , _sTags        = mempty
    , _sKmsKeyId    = Nothing
    }

-- | The description for the snapshot.
sDescription :: Lens' Snapshot Text
sDescription = lens _sDescription (\s a -> s { _sDescription = a })

-- | Indicates whether the snapshot is encrypted.
sEncrypted :: Lens' Snapshot Bool
sEncrypted = lens _sEncrypted (\s a -> s { _sEncrypted = a })

-- | The full ARN of the AWS Key Management Service (KMS) master key that was used
-- to protect the volume encryption key for the parent volume.
sKmsKeyId :: Lens' Snapshot (Maybe Text)
sKmsKeyId = lens _sKmsKeyId (\s a -> s { _sKmsKeyId = a })

-- | The AWS account alias (for example, 'amazon', 'self') or AWS account ID that owns
-- the snapshot.
sOwnerAlias :: Lens' Snapshot (Maybe Text)
sOwnerAlias = lens _sOwnerAlias (\s a -> s { _sOwnerAlias = a })

-- | The AWS account ID of the EBS snapshot owner.
sOwnerId :: Lens' Snapshot Text
sOwnerId = lens _sOwnerId (\s a -> s { _sOwnerId = a })

-- | The progress of the snapshot, as a percentage.
sProgress :: Lens' Snapshot Text
sProgress = lens _sProgress (\s a -> s { _sProgress = a })

-- | The ID of the snapshot.
sSnapshotId :: Lens' Snapshot Text
sSnapshotId = lens _sSnapshotId (\s a -> s { _sSnapshotId = a })

-- | The time stamp when the snapshot was initiated.
sStartTime :: Lens' Snapshot UTCTime
sStartTime = lens _sStartTime (\s a -> s { _sStartTime = a }) . _Time

-- | The snapshot state.
sState :: Lens' Snapshot SnapshotState
sState = lens _sState (\s a -> s { _sState = a })

-- | Any tags assigned to the snapshot.
sTags :: Lens' Snapshot [Tag]
sTags = lens _sTags (\s a -> s { _sTags = a }) . _List

-- | The ID of the volume.
sVolumeId :: Lens' Snapshot Text
sVolumeId = lens _sVolumeId (\s a -> s { _sVolumeId = a })

-- | The size of the volume, in GiB.
sVolumeSize :: Lens' Snapshot Int
sVolumeSize = lens _sVolumeSize (\s a -> s { _sVolumeSize = a })

instance FromXML Snapshot where
    parseXML x = Snapshot
        <$> x .@  "description"
        <*> x .@  "encrypted"
        <*> x .@? "kmsKeyId"
        <*> x .@? "ownerAlias"
        <*> x .@  "ownerId"
        <*> x .@  "progress"
        <*> x .@  "snapshotId"
        <*> x .@  "startTime"
        <*> x .@  "status"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "volumeId"
        <*> x .@  "volumeSize"

instance ToQuery Snapshot where
    toQuery Snapshot{..} = mconcat
        [ "Description" =? _sDescription
        , "Encrypted"   =? _sEncrypted
        , "KmsKeyId"    =? _sKmsKeyId
        , "OwnerAlias"  =? _sOwnerAlias
        , "OwnerId"     =? _sOwnerId
        , "Progress"    =? _sProgress
        , "SnapshotId"  =? _sSnapshotId
        , "StartTime"   =? _sStartTime
        , "Status"      =? _sState
        , "TagSet"      `toQueryList` _sTags
        , "VolumeId"    =? _sVolumeId
        , "VolumeSize"  =? _sVolumeSize
        ]

data SpotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode    :: Maybe Text
    , _sisfMessage :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SpotInstanceStateFault' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sisfCode' @::@ 'Maybe' 'Text'
--
-- * 'sisfMessage' @::@ 'Maybe' 'Text'
--
spotInstanceStateFault :: SpotInstanceStateFault
spotInstanceStateFault = SpotInstanceStateFault
    { _sisfCode    = Nothing
    , _sisfMessage = Nothing
    }

-- | The reason code for the Spot Instance state change.
sisfCode :: Lens' SpotInstanceStateFault (Maybe Text)
sisfCode = lens _sisfCode (\s a -> s { _sisfCode = a })

-- | The message for the Spot Instance state change.
sisfMessage :: Lens' SpotInstanceStateFault (Maybe Text)
sisfMessage = lens _sisfMessage (\s a -> s { _sisfMessage = a })

instance FromXML SpotInstanceStateFault where
    parseXML x = SpotInstanceStateFault
        <$> x .@? "code"
        <*> x .@? "message"

instance ToQuery SpotInstanceStateFault where
    toQuery SpotInstanceStateFault{..} = mconcat
        [ "Code"    =? _sisfCode
        , "Message" =? _sisfMessage
        ]

data TagDescription = TagDescription
    { _tdKey          :: Text
    , _tdResourceId   :: Text
    , _tdResourceType :: ResourceType
    , _tdValue        :: Text
    } deriving (Eq, Read, Show)

-- | 'TagDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdKey' @::@ 'Text'
--
-- * 'tdResourceId' @::@ 'Text'
--
-- * 'tdResourceType' @::@ 'ResourceType'
--
-- * 'tdValue' @::@ 'Text'
--
tagDescription :: Text -- ^ 'tdResourceId'
               -> ResourceType -- ^ 'tdResourceType'
               -> Text -- ^ 'tdKey'
               -> Text -- ^ 'tdValue'
               -> TagDescription
tagDescription p1 p2 p3 p4 = TagDescription
    { _tdResourceId   = p1
    , _tdResourceType = p2
    , _tdKey          = p3
    , _tdValue        = p4
    }

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s { _tdKey = a })

-- | The ID of the resource. For example, 'ami-1a2b3c4d'.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })

-- | The resource type.
tdResourceType :: Lens' TagDescription ResourceType
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s { _tdValue = a })

instance FromXML TagDescription where
    parseXML x = TagDescription
        <$> x .@  "key"
        <*> x .@  "resourceId"
        <*> x .@  "resourceType"
        <*> x .@  "value"

instance ToQuery TagDescription where
    toQuery TagDescription{..} = mconcat
        [ "Key"          =? _tdKey
        , "ResourceId"   =? _tdResourceId
        , "ResourceType" =? _tdResourceType
        , "Value"        =? _tdValue
        ]

data ImportSnapshotTask = ImportSnapshotTask
    { _istDescription        :: Maybe Text
    , _istImportTaskId       :: Maybe Text
    , _istSnapshotTaskDetail :: Maybe SnapshotTaskDetail
    } deriving (Eq, Read, Show)

-- | 'ImportSnapshotTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'istDescription' @::@ 'Maybe' 'Text'
--
-- * 'istImportTaskId' @::@ 'Maybe' 'Text'
--
-- * 'istSnapshotTaskDetail' @::@ 'Maybe' 'SnapshotTaskDetail'
--
importSnapshotTask :: ImportSnapshotTask
importSnapshotTask = ImportSnapshotTask
    { _istImportTaskId       = Nothing
    , _istSnapshotTaskDetail = Nothing
    , _istDescription        = Nothing
    }

-- | A description of the import snapshot task.
istDescription :: Lens' ImportSnapshotTask (Maybe Text)
istDescription = lens _istDescription (\s a -> s { _istDescription = a })

-- | The ID of the import snapshot task.
istImportTaskId :: Lens' ImportSnapshotTask (Maybe Text)
istImportTaskId = lens _istImportTaskId (\s a -> s { _istImportTaskId = a })

-- | Describes an import snapshot task.
istSnapshotTaskDetail :: Lens' ImportSnapshotTask (Maybe SnapshotTaskDetail)
istSnapshotTaskDetail =
    lens _istSnapshotTaskDetail (\s a -> s { _istSnapshotTaskDetail = a })

instance FromXML ImportSnapshotTask where
    parseXML x = ImportSnapshotTask
        <$> x .@? "description"
        <*> x .@? "importTaskId"
        <*> x .@? "snapshotTaskDetail"

instance ToQuery ImportSnapshotTask where
    toQuery ImportSnapshotTask{..} = mconcat
        [ "Description"        =? _istDescription
        , "ImportTaskId"       =? _istImportTaskId
        , "SnapshotTaskDetail" =? _istSnapshotTaskDetail
        ]

data GroupIdentifier = GroupIdentifier
    { _giGroupId   :: Maybe Text
    , _giGroupName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GroupIdentifier' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giGroupId' @::@ 'Maybe' 'Text'
--
-- * 'giGroupName' @::@ 'Maybe' 'Text'
--
groupIdentifier :: GroupIdentifier
groupIdentifier = GroupIdentifier
    { _giGroupName = Nothing
    , _giGroupId   = Nothing
    }

-- | The ID of the security group.
giGroupId :: Lens' GroupIdentifier (Maybe Text)
giGroupId = lens _giGroupId (\s a -> s { _giGroupId = a })

-- | The name of the security group.
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName = lens _giGroupName (\s a -> s { _giGroupName = a })

instance FromXML GroupIdentifier where
    parseXML x = GroupIdentifier
        <$> x .@? "groupId"
        <*> x .@? "groupName"

instance ToQuery GroupIdentifier where
    toQuery GroupIdentifier{..} = mconcat
        [ "GroupId"   =? _giGroupId
        , "GroupName" =? _giGroupName
        ]

data VpnStaticRouteSource
    = Static -- ^ Static
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VpnStaticRouteSource

instance FromText VpnStaticRouteSource where
    parser = takeLowerText >>= \case
        "static" -> pure Static
        e        -> fail $
            "Failure parsing VpnStaticRouteSource from " ++ show e

instance ToText VpnStaticRouteSource where
    toText Static = "Static"

instance ToByteString VpnStaticRouteSource
instance ToHeader     VpnStaticRouteSource
instance ToQuery      VpnStaticRouteSource

instance FromXML VpnStaticRouteSource where
    parseXML = parseXMLText "VpnStaticRouteSource"

data ReservedInstancesListing = ReservedInstancesListing
    { _rilClientToken                :: Maybe Text
    , _rilCreateDate                 :: Maybe ISO8601
    , _rilInstanceCounts             :: List "item" InstanceCount
    , _rilPriceSchedules             :: List "item" PriceSchedule
    , _rilReservedInstancesId        :: Maybe Text
    , _rilReservedInstancesListingId :: Maybe Text
    , _rilStatus                     :: Maybe ListingStatus
    , _rilStatusMessage              :: Maybe Text
    , _rilTags                       :: List "item" Tag
    , _rilUpdateDate                 :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'ReservedInstancesListing' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rilClientToken' @::@ 'Maybe' 'Text'
--
-- * 'rilCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rilInstanceCounts' @::@ ['InstanceCount']
--
-- * 'rilPriceSchedules' @::@ ['PriceSchedule']
--
-- * 'rilReservedInstancesId' @::@ 'Maybe' 'Text'
--
-- * 'rilReservedInstancesListingId' @::@ 'Maybe' 'Text'
--
-- * 'rilStatus' @::@ 'Maybe' 'ListingStatus'
--
-- * 'rilStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'rilTags' @::@ ['Tag']
--
-- * 'rilUpdateDate' @::@ 'Maybe' 'UTCTime'
--
reservedInstancesListing :: ReservedInstancesListing
reservedInstancesListing = ReservedInstancesListing
    { _rilReservedInstancesListingId = Nothing
    , _rilReservedInstancesId        = Nothing
    , _rilCreateDate                 = Nothing
    , _rilUpdateDate                 = Nothing
    , _rilStatus                     = Nothing
    , _rilStatusMessage              = Nothing
    , _rilInstanceCounts             = mempty
    , _rilPriceSchedules             = mempty
    , _rilTags                       = mempty
    , _rilClientToken                = Nothing
    }

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
rilClientToken :: Lens' ReservedInstancesListing (Maybe Text)
rilClientToken = lens _rilClientToken (\s a -> s { _rilClientToken = a })

-- | The time the listing was created.
rilCreateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilCreateDate = lens _rilCreateDate (\s a -> s { _rilCreateDate = a }) . mapping _Time

-- | The number of instances in this state.
rilInstanceCounts :: Lens' ReservedInstancesListing [InstanceCount]
rilInstanceCounts =
    lens _rilInstanceCounts (\s a -> s { _rilInstanceCounts = a })
        . _List

-- | The price of the Reserved Instance listing.
rilPriceSchedules :: Lens' ReservedInstancesListing [PriceSchedule]
rilPriceSchedules =
    lens _rilPriceSchedules (\s a -> s { _rilPriceSchedules = a })
        . _List

-- | The ID of the Reserved Instance.
rilReservedInstancesId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesId =
    lens _rilReservedInstancesId (\s a -> s { _rilReservedInstancesId = a })

-- | The ID of the Reserved Instance listing.
rilReservedInstancesListingId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesListingId =
    lens _rilReservedInstancesListingId
        (\s a -> s { _rilReservedInstancesListingId = a })

-- | The status of the Reserved Instance listing.
rilStatus :: Lens' ReservedInstancesListing (Maybe ListingStatus)
rilStatus = lens _rilStatus (\s a -> s { _rilStatus = a })

-- | The reason for the current status of the Reserved Instance listing. The
-- response can be blank.
rilStatusMessage :: Lens' ReservedInstancesListing (Maybe Text)
rilStatusMessage = lens _rilStatusMessage (\s a -> s { _rilStatusMessage = a })

-- | Any tags assigned to the resource.
rilTags :: Lens' ReservedInstancesListing [Tag]
rilTags = lens _rilTags (\s a -> s { _rilTags = a }) . _List

-- | The last modified timestamp of the listing.
rilUpdateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilUpdateDate = lens _rilUpdateDate (\s a -> s { _rilUpdateDate = a }) . mapping _Time

instance FromXML ReservedInstancesListing where
    parseXML x = ReservedInstancesListing
        <$> x .@? "clientToken"
        <*> x .@? "createDate"
        <*> x .@? "instanceCounts" .!@ mempty
        <*> x .@? "priceSchedules" .!@ mempty
        <*> x .@? "reservedInstancesId"
        <*> x .@? "reservedInstancesListingId"
        <*> x .@? "status"
        <*> x .@? "statusMessage"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "updateDate"

instance ToQuery ReservedInstancesListing where
    toQuery ReservedInstancesListing{..} = mconcat
        [ "ClientToken"                =? _rilClientToken
        , "CreateDate"                 =? _rilCreateDate
        , "InstanceCounts"             `toQueryList` _rilInstanceCounts
        , "PriceSchedules"             `toQueryList` _rilPriceSchedules
        , "ReservedInstancesId"        =? _rilReservedInstancesId
        , "ReservedInstancesListingId" =? _rilReservedInstancesListingId
        , "Status"                     =? _rilStatus
        , "StatusMessage"              =? _rilStatusMessage
        , "TagSet"                     `toQueryList` _rilTags
        , "UpdateDate"                 =? _rilUpdateDate
        ]

data InstanceLifecycleType
    = Spot -- ^ spot
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable InstanceLifecycleType

instance FromText InstanceLifecycleType where
    parser = takeLowerText >>= \case
        "spot" -> pure Spot
        e      -> fail $
            "Failure parsing InstanceLifecycleType from " ++ show e

instance ToText InstanceLifecycleType where
    toText Spot = "spot"

instance ToByteString InstanceLifecycleType
instance ToHeader     InstanceLifecycleType
instance ToQuery      InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    parseXML = parseXMLText "InstanceLifecycleType"

data State
    = Available -- ^ Available
    | Deleted   -- ^ Deleted
    | Deleting  -- ^ Deleting
    | Pending   -- ^ Pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable State

instance FromText State where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "deleted"   -> pure Deleted
        "deleting"  -> pure Deleting
        "pending"   -> pure Pending
        e           -> fail $
            "Failure parsing State from " ++ show e

instance ToText State where
    toText = \case
        Available -> "Available"
        Deleted   -> "Deleted"
        Deleting  -> "Deleting"
        Pending   -> "Pending"

instance ToByteString State
instance ToHeader     State
instance ToQuery      State

instance FromXML State where
    parseXML = parseXMLText "State"

data VirtualizationType
    = Hvm         -- ^ hvm
    | Paravirtual -- ^ paravirtual
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VirtualizationType

instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm"         -> pure Hvm
        "paravirtual" -> pure Paravirtual
        e             -> fail $
            "Failure parsing VirtualizationType from " ++ show e

instance ToText VirtualizationType where
    toText = \case
        Hvm         -> "hvm"
        Paravirtual -> "paravirtual"

instance ToByteString VirtualizationType
instance ToHeader     VirtualizationType
instance ToQuery      VirtualizationType

instance FromXML VirtualizationType where
    parseXML = parseXMLText "VirtualizationType"

data NetworkInterfaceStatus
    = NISAttaching -- ^ attaching
    | NISAvailable -- ^ available
    | NISDetaching -- ^ detaching
    | NISInUse     -- ^ in-use
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable NetworkInterfaceStatus

instance FromText NetworkInterfaceStatus where
    parser = takeLowerText >>= \case
        "attaching" -> pure NISAttaching
        "available" -> pure NISAvailable
        "detaching" -> pure NISDetaching
        "in-use"    -> pure NISInUse
        e           -> fail $
            "Failure parsing NetworkInterfaceStatus from " ++ show e

instance ToText NetworkInterfaceStatus where
    toText = \case
        NISAttaching -> "attaching"
        NISAvailable -> "available"
        NISDetaching -> "detaching"
        NISInUse     -> "in-use"

instance ToByteString NetworkInterfaceStatus
instance ToHeader     NetworkInterfaceStatus
instance ToQuery      NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
    parseXML = parseXMLText "NetworkInterfaceStatus"

data PlatformValues
    = Windows -- ^ Windows
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable PlatformValues

instance FromText PlatformValues where
    parser = takeLowerText >>= \case
        "windows" -> pure Windows
        e         -> fail $
            "Failure parsing PlatformValues from " ++ show e

instance ToText PlatformValues where
    toText Windows = "Windows"

instance ToByteString PlatformValues
instance ToHeader     PlatformValues
instance ToQuery      PlatformValues

instance FromXML PlatformValues where
    parseXML = parseXMLText "PlatformValues"

data CreateVolumePermission = CreateVolumePermission
    { _cvpGroup  :: Maybe PermissionGroup
    , _cvpUserId :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'CreateVolumePermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpGroup' @::@ 'Maybe' 'PermissionGroup'
--
-- * 'cvpUserId' @::@ 'Maybe' 'Text'
--
createVolumePermission :: CreateVolumePermission
createVolumePermission = CreateVolumePermission
    { _cvpUserId = Nothing
    , _cvpGroup  = Nothing
    }

-- | The specific group that is to be added or removed from a volume's list of
-- create volume permissions.
cvpGroup :: Lens' CreateVolumePermission (Maybe PermissionGroup)
cvpGroup = lens _cvpGroup (\s a -> s { _cvpGroup = a })

-- | The specific AWS account ID that is to be added or removed from a volume's
-- list of create volume permissions.
cvpUserId :: Lens' CreateVolumePermission (Maybe Text)
cvpUserId = lens _cvpUserId (\s a -> s { _cvpUserId = a })

instance FromXML CreateVolumePermission where
    parseXML x = CreateVolumePermission
        <$> x .@? "group"
        <*> x .@? "userId"

instance ToQuery CreateVolumePermission where
    toQuery CreateVolumePermission{..} = mconcat
        [ "Group"  =? _cvpGroup
        , "UserId" =? _cvpUserId
        ]

data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId        :: Maybe Text
    , _niacDeleteOnTermination :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'NetworkInterfaceAttachmentChanges' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niacAttachmentId' @::@ 'Maybe' 'Text'
--
-- * 'niacDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
networkInterfaceAttachmentChanges :: NetworkInterfaceAttachmentChanges
networkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { _niacAttachmentId        = Nothing
    , _niacDeleteOnTermination = Nothing
    }

-- | The ID of the network interface attachment.
niacAttachmentId :: Lens' NetworkInterfaceAttachmentChanges (Maybe Text)
niacAttachmentId = lens _niacAttachmentId (\s a -> s { _niacAttachmentId = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niacDeleteOnTermination :: Lens' NetworkInterfaceAttachmentChanges (Maybe Bool)
niacDeleteOnTermination =
    lens _niacDeleteOnTermination (\s a -> s { _niacDeleteOnTermination = a })

instance FromXML NetworkInterfaceAttachmentChanges where
    parseXML x = NetworkInterfaceAttachmentChanges
        <$> x .@? "attachmentId"
        <*> x .@? "deleteOnTermination"

instance ToQuery NetworkInterfaceAttachmentChanges where
    toQuery NetworkInterfaceAttachmentChanges{..} = mconcat
        [ "AttachmentId"        =? _niacAttachmentId
        , "DeleteOnTermination" =? _niacDeleteOnTermination
        ]

data RecurringChargeFrequency
    = Hourly -- ^ Hourly
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable RecurringChargeFrequency

instance FromText RecurringChargeFrequency where
    parser = takeLowerText >>= \case
        "hourly" -> pure Hourly
        e        -> fail $
            "Failure parsing RecurringChargeFrequency from " ++ show e

instance ToText RecurringChargeFrequency where
    toText Hourly = "Hourly"

instance ToByteString RecurringChargeFrequency
instance ToHeader     RecurringChargeFrequency
instance ToQuery      RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
    parseXML = parseXMLText "RecurringChargeFrequency"

data DhcpOptions = DhcpOptions
    { _doDhcpConfigurations :: List "item" DhcpConfiguration
    , _doDhcpOptionsId      :: Maybe Text
    , _doTags               :: List "item" Tag
    } deriving (Eq, Read, Show)

-- | 'DhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doDhcpConfigurations' @::@ ['DhcpConfiguration']
--
-- * 'doDhcpOptionsId' @::@ 'Maybe' 'Text'
--
-- * 'doTags' @::@ ['Tag']
--
dhcpOptions :: DhcpOptions
dhcpOptions = DhcpOptions
    { _doDhcpOptionsId      = Nothing
    , _doDhcpConfigurations = mempty
    , _doTags               = mempty
    }

-- | One or more DHCP options in the set.
doDhcpConfigurations :: Lens' DhcpOptions [DhcpConfiguration]
doDhcpConfigurations =
    lens _doDhcpConfigurations (\s a -> s { _doDhcpConfigurations = a })
        . _List

-- | The ID of the set of DHCP options.
doDhcpOptionsId :: Lens' DhcpOptions (Maybe Text)
doDhcpOptionsId = lens _doDhcpOptionsId (\s a -> s { _doDhcpOptionsId = a })

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DhcpOptions [Tag]
doTags = lens _doTags (\s a -> s { _doTags = a }) . _List

instance FromXML DhcpOptions where
    parseXML x = DhcpOptions
        <$> x .@? "dhcpConfigurationSet" .!@ mempty
        <*> x .@? "dhcpOptionsId"
        <*> x .@? "tagSet" .!@ mempty

instance ToQuery DhcpOptions where
    toQuery DhcpOptions{..} = mconcat
        [ "DhcpConfigurationSet" `toQueryList` _doDhcpConfigurations
        , "DhcpOptionsId"        =? _doDhcpOptionsId
        , "TagSet"               `toQueryList` _doTags
        ]

data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisAssociatePublicIpAddress       :: Maybe Bool
    , _inisDeleteOnTermination            :: Maybe Bool
    , _inisDescription                    :: Maybe Text
    , _inisDeviceIndex                    :: Maybe Int
    , _inisGroups                         :: List "SecurityGroupId" Text
    , _inisNetworkInterfaceId             :: Maybe Text
    , _inisPrivateIpAddress               :: Maybe Text
    , _inisPrivateIpAddresses             :: List "item" PrivateIpAddressSpecification
    , _inisSecondaryPrivateIpAddressCount :: Maybe Int
    , _inisSubnetId                       :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'InstanceNetworkInterfaceSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'inisAssociatePublicIpAddress' @::@ 'Maybe' 'Bool'
--
-- * 'inisDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'inisDescription' @::@ 'Maybe' 'Text'
--
-- * 'inisDeviceIndex' @::@ 'Maybe' 'Int'
--
-- * 'inisGroups' @::@ ['Text']
--
-- * 'inisNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'inisPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'inisPrivateIpAddresses' @::@ ['PrivateIpAddressSpecification']
--
-- * 'inisSecondaryPrivateIpAddressCount' @::@ 'Maybe' 'Int'
--
-- * 'inisSubnetId' @::@ 'Maybe' 'Text'
--
instanceNetworkInterfaceSpecification :: InstanceNetworkInterfaceSpecification
instanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { _inisNetworkInterfaceId             = Nothing
    , _inisDeviceIndex                    = Nothing
    , _inisSubnetId                       = Nothing
    , _inisDescription                    = Nothing
    , _inisPrivateIpAddress               = Nothing
    , _inisGroups                         = mempty
    , _inisDeleteOnTermination            = Nothing
    , _inisPrivateIpAddresses             = mempty
    , _inisSecondaryPrivateIpAddressCount = Nothing
    , _inisAssociatePublicIpAddress       = Nothing
    }

-- | Indicates whether to assign a public IP address to an instance you launch in
-- a VPC. The public IP address can only be assigned to a network interface for
-- eth0, and can only be assigned to a new network interface, not an existing
-- one. You cannot specify more than one network interface in the request. If
-- launching into a default subnet, the default value is 'true'.
inisAssociatePublicIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisAssociatePublicIpAddress =
    lens _inisAssociatePublicIpAddress
        (\s a -> s { _inisAssociatePublicIpAddress = a })

-- | If set to 'true', the interface is deleted when the instance is terminated. You
-- can specify 'true' only if creating a new network interface when launching an
-- instance.
inisDeleteOnTermination :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisDeleteOnTermination =
    lens _inisDeleteOnTermination (\s a -> s { _inisDeleteOnTermination = a })

-- | The description of the network interface. Applies only if creating a network
-- interface when launching an instance.
inisDescription :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisDescription = lens _inisDescription (\s a -> s { _inisDescription = a })

-- | The index of the device on the instance for the network interface attachment.
-- If you are specifying a network interface in a 'RunInstances' request, you must
-- provide the device index.
inisDeviceIndex :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisDeviceIndex = lens _inisDeviceIndex (\s a -> s { _inisDeviceIndex = a })

-- | The IDs of the security groups for the network interface. Applies only if
-- creating a network interface when launching an instance.
inisGroups :: Lens' InstanceNetworkInterfaceSpecification [Text]
inisGroups = lens _inisGroups (\s a -> s { _inisGroups = a }) . _List

-- | The ID of the network interface.
inisNetworkInterfaceId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisNetworkInterfaceId =
    lens _inisNetworkInterfaceId (\s a -> s { _inisNetworkInterfaceId = a })

-- | The private IP address of the network interface. Applies only if creating a
-- network interface when launching an instance.
inisPrivateIpAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisPrivateIpAddress =
    lens _inisPrivateIpAddress (\s a -> s { _inisPrivateIpAddress = a })

-- | One or more private IP addresses to assign to the network interface. Only one
-- private IP address can be designated as primary.
inisPrivateIpAddresses :: Lens' InstanceNetworkInterfaceSpecification [PrivateIpAddressSpecification]
inisPrivateIpAddresses =
    lens _inisPrivateIpAddresses (\s a -> s { _inisPrivateIpAddresses = a })
        . _List

-- | The number of secondary private IP addresses. You can't specify this option
-- and specify more than one private IP address using the private IP addresses
-- option.
inisSecondaryPrivateIpAddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisSecondaryPrivateIpAddressCount =
    lens _inisSecondaryPrivateIpAddressCount
        (\s a -> s { _inisSecondaryPrivateIpAddressCount = a })

-- | The ID of the subnet associated with the network string. Applies only if
-- creating a network interface when launching an instance.
inisSubnetId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisSubnetId = lens _inisSubnetId (\s a -> s { _inisSubnetId = a })

instance FromXML InstanceNetworkInterfaceSpecification where
    parseXML x = InstanceNetworkInterfaceSpecification
        <$> x .@? "associatePublicIpAddress"
        <*> x .@? "deleteOnTermination"
        <*> x .@? "description"
        <*> x .@? "deviceIndex"
        <*> x .@? "SecurityGroupId" .!@ mempty
        <*> x .@? "networkInterfaceId"
        <*> x .@? "privateIpAddress"
        <*> x .@? "privateIpAddressesSet" .!@ mempty
        <*> x .@? "secondaryPrivateIpAddressCount"
        <*> x .@? "subnetId"

instance ToQuery InstanceNetworkInterfaceSpecification where
    toQuery InstanceNetworkInterfaceSpecification{..} = mconcat
        [ "AssociatePublicIpAddress"       =? _inisAssociatePublicIpAddress
        , "DeleteOnTermination"            =? _inisDeleteOnTermination
        , "Description"                    =? _inisDescription
        , "DeviceIndex"                    =? _inisDeviceIndex
        , "SecurityGroupId"                `toQueryList` _inisGroups
        , "NetworkInterfaceId"             =? _inisNetworkInterfaceId
        , "PrivateIpAddress"               =? _inisPrivateIpAddress
        , "PrivateIpAddressesSet"          `toQueryList` _inisPrivateIpAddresses
        , "SecondaryPrivateIpAddressCount" =? _inisSecondaryPrivateIpAddressCount
        , "SubnetId"                       =? _inisSubnetId
        ]

data VolumeState
    = VSAvailable -- ^ available
    | VSCreating  -- ^ creating
    | VSDeleted   -- ^ deleted
    | VSDeleting  -- ^ deleting
    | VSError     -- ^ error
    | VSInUse     -- ^ in-use
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VolumeState

instance FromText VolumeState where
    parser = takeLowerText >>= \case
        "available" -> pure VSAvailable
        "creating"  -> pure VSCreating
        "deleted"   -> pure VSDeleted
        "deleting"  -> pure VSDeleting
        "error"     -> pure VSError
        "in-use"    -> pure VSInUse
        e           -> fail $
            "Failure parsing VolumeState from " ++ show e

instance ToText VolumeState where
    toText = \case
        VSAvailable -> "available"
        VSCreating  -> "creating"
        VSDeleted   -> "deleted"
        VSDeleting  -> "deleting"
        VSError     -> "error"
        VSInUse     -> "in-use"

instance ToByteString VolumeState
instance ToHeader     VolumeState
instance ToQuery      VolumeState

instance FromXML VolumeState where
    parseXML = parseXMLText "VolumeState"

newtype AttributeValue = AttributeValue
    { _avValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avValue' @::@ 'Maybe' 'Text'
--
attributeValue :: AttributeValue
attributeValue = AttributeValue
    { _avValue = Nothing
    }

-- | Valid values are case-sensitive and vary by action.
avValue :: Lens' AttributeValue (Maybe Text)
avValue = lens _avValue (\s a -> s { _avValue = a })

instance FromXML AttributeValue where
    parseXML x = AttributeValue
        <$> x .@? "value"

instance ToQuery AttributeValue where
    toQuery AttributeValue{..} = mconcat
        [ "Value" =? _avValue
        ]

data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { _piasPrimary          :: Maybe Bool
    , _piasPrivateIpAddress :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PrivateIpAddressSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'piasPrimary' @::@ 'Maybe' 'Bool'
--
-- * 'piasPrivateIpAddress' @::@ 'Text'
--
privateIpAddressSpecification :: Text -- ^ 'piasPrivateIpAddress'
                              -> PrivateIpAddressSpecification
privateIpAddressSpecification p1 = PrivateIpAddressSpecification
    { _piasPrivateIpAddress = p1
    , _piasPrimary          = Nothing
    }

-- | Indicates whether the private IP address is the primary private IP address.
-- Only one IP address can be designated as primary.
piasPrimary :: Lens' PrivateIpAddressSpecification (Maybe Bool)
piasPrimary = lens _piasPrimary (\s a -> s { _piasPrimary = a })

-- | The private IP addresses.
piasPrivateIpAddress :: Lens' PrivateIpAddressSpecification Text
piasPrivateIpAddress =
    lens _piasPrivateIpAddress (\s a -> s { _piasPrivateIpAddress = a })

instance FromXML PrivateIpAddressSpecification where
    parseXML x = PrivateIpAddressSpecification
        <$> x .@? "primary"
        <*> x .@  "privateIpAddress"

instance ToQuery PrivateIpAddressSpecification where
    toQuery PrivateIpAddressSpecification{..} = mconcat
        [ "Primary"          =? _piasPrimary
        , "PrivateIpAddress" =? _piasPrivateIpAddress
        ]

data Image = Image
    { _iArchitecture        :: ArchitectureValues
    , _iBlockDeviceMappings :: List "item" BlockDeviceMapping
    , _iCreationDate        :: Maybe Text
    , _iDescription         :: Maybe Text
    , _iHypervisor          :: HypervisorType
    , _iImageId             :: Text
    , _iImageLocation       :: Text
    , _iImageOwnerAlias     :: Maybe Text
    , _iImageType           :: ImageTypeValues
    , _iKernelId            :: Maybe Text
    , _iName                :: Maybe Text
    , _iOwnerId             :: Text
    , _iPlatform            :: Maybe PlatformValues
    , _iProductCodes        :: List "item" ProductCode
    , _iPublic              :: Bool
    , _iRamdiskId           :: Maybe Text
    , _iRootDeviceName      :: Maybe Text
    , _iRootDeviceType      :: DeviceType
    , _iSriovNetSupport     :: Maybe Text
    , _iState               :: ImageState
    , _iStateReason         :: Maybe StateReason
    , _iTags                :: List "item" Tag
    , _iVirtualizationType  :: VirtualizationType
    } deriving (Eq, Read, Show)

-- | 'Image' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iArchitecture' @::@ 'ArchitectureValues'
--
-- * 'iBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'iCreationDate' @::@ 'Maybe' 'Text'
--
-- * 'iDescription' @::@ 'Maybe' 'Text'
--
-- * 'iHypervisor' @::@ 'HypervisorType'
--
-- * 'iImageId' @::@ 'Text'
--
-- * 'iImageLocation' @::@ 'Text'
--
-- * 'iImageOwnerAlias' @::@ 'Maybe' 'Text'
--
-- * 'iImageType' @::@ 'ImageTypeValues'
--
-- * 'iKernelId' @::@ 'Maybe' 'Text'
--
-- * 'iName' @::@ 'Maybe' 'Text'
--
-- * 'iOwnerId' @::@ 'Text'
--
-- * 'iPlatform' @::@ 'Maybe' 'PlatformValues'
--
-- * 'iProductCodes' @::@ ['ProductCode']
--
-- * 'iPublic' @::@ 'Bool'
--
-- * 'iRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'iRootDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'iRootDeviceType' @::@ 'DeviceType'
--
-- * 'iSriovNetSupport' @::@ 'Maybe' 'Text'
--
-- * 'iState' @::@ 'ImageState'
--
-- * 'iStateReason' @::@ 'Maybe' 'StateReason'
--
-- * 'iTags' @::@ ['Tag']
--
-- * 'iVirtualizationType' @::@ 'VirtualizationType'
--
image :: Text -- ^ 'iImageId'
      -> Text -- ^ 'iImageLocation'
      -> ImageState -- ^ 'iState'
      -> Text -- ^ 'iOwnerId'
      -> Bool -- ^ 'iPublic'
      -> ArchitectureValues -- ^ 'iArchitecture'
      -> ImageTypeValues -- ^ 'iImageType'
      -> DeviceType -- ^ 'iRootDeviceType'
      -> VirtualizationType -- ^ 'iVirtualizationType'
      -> HypervisorType -- ^ 'iHypervisor'
      -> Image
image p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 = Image
    { _iImageId             = p1
    , _iImageLocation       = p2
    , _iState               = p3
    , _iOwnerId             = p4
    , _iPublic              = p5
    , _iArchitecture        = p6
    , _iImageType           = p7
    , _iRootDeviceType      = p8
    , _iVirtualizationType  = p9
    , _iHypervisor          = p10
    , _iCreationDate        = Nothing
    , _iProductCodes        = mempty
    , _iKernelId            = Nothing
    , _iRamdiskId           = Nothing
    , _iPlatform            = Nothing
    , _iSriovNetSupport     = Nothing
    , _iStateReason         = Nothing
    , _iImageOwnerAlias     = Nothing
    , _iName                = Nothing
    , _iDescription         = Nothing
    , _iRootDeviceName      = Nothing
    , _iBlockDeviceMappings = mempty
    , _iTags                = mempty
    }

-- | The architecture of the image.
iArchitecture :: Lens' Image ArchitectureValues
iArchitecture = lens _iArchitecture (\s a -> s { _iArchitecture = a })

-- | Any block device mapping entries.
iBlockDeviceMappings :: Lens' Image [BlockDeviceMapping]
iBlockDeviceMappings =
    lens _iBlockDeviceMappings (\s a -> s { _iBlockDeviceMappings = a })
        . _List

-- | The date and time the image was created.
iCreationDate :: Lens' Image (Maybe Text)
iCreationDate = lens _iCreationDate (\s a -> s { _iCreationDate = a })

-- | The description of the AMI that was provided during image creation.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\s a -> s { _iDescription = a })

-- | The hypervisor type of the image.
iHypervisor :: Lens' Image HypervisorType
iHypervisor = lens _iHypervisor (\s a -> s { _iHypervisor = a })

-- | The ID of the AMI.
iImageId :: Lens' Image Text
iImageId = lens _iImageId (\s a -> s { _iImageId = a })

-- | The location of the AMI.
iImageLocation :: Lens' Image Text
iImageLocation = lens _iImageLocation (\s a -> s { _iImageLocation = a })

-- | The AWS account alias (for example, 'amazon', 'self') or the AWS account ID of
-- the AMI owner.
iImageOwnerAlias :: Lens' Image (Maybe Text)
iImageOwnerAlias = lens _iImageOwnerAlias (\s a -> s { _iImageOwnerAlias = a })

-- | The type of image.
iImageType :: Lens' Image ImageTypeValues
iImageType = lens _iImageType (\s a -> s { _iImageType = a })

-- | The kernel associated with the image, if any. Only applicable for machine
-- images.
iKernelId :: Lens' Image (Maybe Text)
iKernelId = lens _iKernelId (\s a -> s { _iKernelId = a })

-- | The name of the AMI that was provided during image creation.
iName :: Lens' Image (Maybe Text)
iName = lens _iName (\s a -> s { _iName = a })

-- | The AWS account ID of the image owner.
iOwnerId :: Lens' Image Text
iOwnerId = lens _iOwnerId (\s a -> s { _iOwnerId = a })

-- | The value is 'Windows' for Windows AMIs; otherwise blank.
iPlatform :: Lens' Image (Maybe PlatformValues)
iPlatform = lens _iPlatform (\s a -> s { _iPlatform = a })

-- | Any product codes associated with the AMI.
iProductCodes :: Lens' Image [ProductCode]
iProductCodes = lens _iProductCodes (\s a -> s { _iProductCodes = a }) . _List

-- | Indicates whether the image has public launch permissions. The value is 'true'
-- if this image has public launch permissions or 'false' if it has only implicit
-- and explicit launch permissions.
iPublic :: Lens' Image Bool
iPublic = lens _iPublic (\s a -> s { _iPublic = a })

-- | The RAM disk associated with the image, if any. Only applicable for machine
-- images.
iRamdiskId :: Lens' Image (Maybe Text)
iRamdiskId = lens _iRamdiskId (\s a -> s { _iRamdiskId = a })

-- | The device name of the root device (for example, '/dev/sda1' or '/dev/xvda').
iRootDeviceName :: Lens' Image (Maybe Text)
iRootDeviceName = lens _iRootDeviceName (\s a -> s { _iRootDeviceName = a })

-- | The type of root device used by the AMI. The AMI can use an EBS volume or an
-- instance store volume.
iRootDeviceType :: Lens' Image DeviceType
iRootDeviceType = lens _iRootDeviceType (\s a -> s { _iRootDeviceType = a })

-- | Specifies whether enhanced networking is enabled.
iSriovNetSupport :: Lens' Image (Maybe Text)
iSriovNetSupport = lens _iSriovNetSupport (\s a -> s { _iSriovNetSupport = a })

-- | The current state of the AMI. If the state is 'available', the image is
-- successfully registered and can be used to launch an instance.
iState :: Lens' Image ImageState
iState = lens _iState (\s a -> s { _iState = a })

-- | The reason for the state change.
iStateReason :: Lens' Image (Maybe StateReason)
iStateReason = lens _iStateReason (\s a -> s { _iStateReason = a })

-- | Any tags assigned to the image.
iTags :: Lens' Image [Tag]
iTags = lens _iTags (\s a -> s { _iTags = a }) . _List

-- | The type of virtualization of the AMI.
iVirtualizationType :: Lens' Image VirtualizationType
iVirtualizationType =
    lens _iVirtualizationType (\s a -> s { _iVirtualizationType = a })

instance FromXML Image where
    parseXML x = Image
        <$> x .@  "architecture"
        <*> x .@? "blockDeviceMapping" .!@ mempty
        <*> x .@? "creationDate"
        <*> x .@? "description"
        <*> x .@  "hypervisor"
        <*> x .@  "imageId"
        <*> x .@  "imageLocation"
        <*> x .@? "imageOwnerAlias"
        <*> x .@  "imageType"
        <*> x .@? "kernelId"
        <*> x .@? "name"
        <*> x .@  "imageOwnerId"
        <*> x .@? "platform"
        <*> x .@? "productCodes" .!@ mempty
        <*> x .@  "isPublic"
        <*> x .@? "ramdiskId"
        <*> x .@? "rootDeviceName"
        <*> x .@  "rootDeviceType"
        <*> x .@? "sriovNetSupport"
        <*> x .@  "imageState"
        <*> x .@? "stateReason"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "virtualizationType"

instance ToQuery Image where
    toQuery Image{..} = mconcat
        [ "Architecture"       =? _iArchitecture
        , "BlockDeviceMapping" `toQueryList` _iBlockDeviceMappings
        , "CreationDate"       =? _iCreationDate
        , "Description"        =? _iDescription
        , "Hypervisor"         =? _iHypervisor
        , "ImageId"            =? _iImageId
        , "ImageLocation"      =? _iImageLocation
        , "ImageOwnerAlias"    =? _iImageOwnerAlias
        , "ImageType"          =? _iImageType
        , "KernelId"           =? _iKernelId
        , "Name"               =? _iName
        , "ImageOwnerId"       =? _iOwnerId
        , "Platform"           =? _iPlatform
        , "ProductCodes"       `toQueryList` _iProductCodes
        , "IsPublic"           =? _iPublic
        , "RamdiskId"          =? _iRamdiskId
        , "RootDeviceName"     =? _iRootDeviceName
        , "RootDeviceType"     =? _iRootDeviceType
        , "SriovNetSupport"    =? _iSriovNetSupport
        , "ImageState"         =? _iState
        , "StateReason"        =? _iStateReason
        , "TagSet"             `toQueryList` _iTags
        , "VirtualizationType" =? _iVirtualizationType
        ]

data DhcpConfiguration = DhcpConfiguration
    { _dcKey    :: Maybe Text
    , _dcValues :: List "item" AttributeValue
    } deriving (Eq, Read, Show)

-- | 'DhcpConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcKey' @::@ 'Maybe' 'Text'
--
-- * 'dcValues' @::@ ['AttributeValue']
--
dhcpConfiguration :: DhcpConfiguration
dhcpConfiguration = DhcpConfiguration
    { _dcKey    = Nothing
    , _dcValues = mempty
    }

-- | The name of a DHCP option.
dcKey :: Lens' DhcpConfiguration (Maybe Text)
dcKey = lens _dcKey (\s a -> s { _dcKey = a })

-- | One or more values for the DHCP option.
dcValues :: Lens' DhcpConfiguration [AttributeValue]
dcValues = lens _dcValues (\s a -> s { _dcValues = a }) . _List

instance FromXML DhcpConfiguration where
    parseXML x = DhcpConfiguration
        <$> x .@? "key"
        <*> x .@? "valueSet" .!@ mempty

instance ToQuery DhcpConfiguration where
    toQuery DhcpConfiguration{..} = mconcat
        [ "Key"      =? _dcKey
        , "ValueSet" `toQueryList` _dcValues
        ]

data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError
    { _csfreCode    :: CancelBatchErrorCode
    , _csfreMessage :: Text
    } deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsError' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfreCode' @::@ 'CancelBatchErrorCode'
--
-- * 'csfreMessage' @::@ 'Text'
--
cancelSpotFleetRequestsError :: CancelBatchErrorCode -- ^ 'csfreCode'
                             -> Text -- ^ 'csfreMessage'
                             -> CancelSpotFleetRequestsError
cancelSpotFleetRequestsError p1 p2 = CancelSpotFleetRequestsError
    { _csfreCode    = p1
    , _csfreMessage = p2
    }

-- | The error code.
csfreCode :: Lens' CancelSpotFleetRequestsError CancelBatchErrorCode
csfreCode = lens _csfreCode (\s a -> s { _csfreCode = a })

-- | The description for the error code.
csfreMessage :: Lens' CancelSpotFleetRequestsError Text
csfreMessage = lens _csfreMessage (\s a -> s { _csfreMessage = a })

instance FromXML CancelSpotFleetRequestsError where
    parseXML x = CancelSpotFleetRequestsError
        <$> x .@  "code"
        <*> x .@  "message"

instance ToQuery CancelSpotFleetRequestsError where
    toQuery CancelSpotFleetRequestsError{..} = mconcat
        [ "Code"    =? _csfreCode
        , "Message" =? _csfreMessage
        ]

data Tag = Tag
    { _tagKey   :: Text
    , _tagValue :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Text'
--
-- * 'tagValue' @::@ 'Text'
--
tag :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag p1 p2 = Tag
    { _tagKey   = p1
    , _tagValue = p2
    }

-- | The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127 Unicode
-- characters. May not begin with 'aws:'
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255
-- Unicode characters.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    parseXML x = Tag
        <$> x .@  "key"
        <*> x .@  "value"

instance ToQuery Tag where
    toQuery Tag{..} = mconcat
        [ "Key"   =? _tagKey
        , "Value" =? _tagValue
        ]

data AccountAttributeName
    = DefaultVpc         -- ^ default-vpc
    | SupportedPlatforms -- ^ supported-platforms
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AccountAttributeName

instance FromText AccountAttributeName where
    parser = takeLowerText >>= \case
        "default-vpc"         -> pure DefaultVpc
        "supported-platforms" -> pure SupportedPlatforms
        e                     -> fail $
            "Failure parsing AccountAttributeName from " ++ show e

instance ToText AccountAttributeName where
    toText = \case
        DefaultVpc         -> "default-vpc"
        SupportedPlatforms -> "supported-platforms"

instance ToByteString AccountAttributeName
instance ToHeader     AccountAttributeName
instance ToQuery      AccountAttributeName

instance FromXML AccountAttributeName where
    parseXML = parseXMLText "AccountAttributeName"

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaAttachTime          :: Maybe ISO8601
    , _niaAttachmentId        :: Maybe Text
    , _niaDeleteOnTermination :: Maybe Bool
    , _niaDeviceIndex         :: Maybe Int
    , _niaInstanceId          :: Maybe Text
    , _niaInstanceOwnerId     :: Maybe Text
    , _niaStatus              :: Maybe AttachmentStatus
    } deriving (Eq, Read, Show)

-- | 'NetworkInterfaceAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niaAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'niaAttachmentId' @::@ 'Maybe' 'Text'
--
-- * 'niaDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'niaDeviceIndex' @::@ 'Maybe' 'Int'
--
-- * 'niaInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'niaInstanceOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'niaStatus' @::@ 'Maybe' 'AttachmentStatus'
--
networkInterfaceAttachment :: NetworkInterfaceAttachment
networkInterfaceAttachment = NetworkInterfaceAttachment
    { _niaAttachmentId        = Nothing
    , _niaInstanceId          = Nothing
    , _niaInstanceOwnerId     = Nothing
    , _niaDeviceIndex         = Nothing
    , _niaStatus              = Nothing
    , _niaAttachTime          = Nothing
    , _niaDeleteOnTermination = Nothing
    }

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe UTCTime)
niaAttachTime = lens _niaAttachTime (\s a -> s { _niaAttachTime = a }) . mapping _Time

-- | The ID of the network interface attachment.
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId = lens _niaAttachmentId (\s a -> s { _niaAttachmentId = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination =
    lens _niaDeleteOnTermination (\s a -> s { _niaDeleteOnTermination = a })

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Int)
niaDeviceIndex = lens _niaDeviceIndex (\s a -> s { _niaDeviceIndex = a })

-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId = lens _niaInstanceId (\s a -> s { _niaInstanceId = a })

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId =
    lens _niaInstanceOwnerId (\s a -> s { _niaInstanceOwnerId = a })

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe AttachmentStatus)
niaStatus = lens _niaStatus (\s a -> s { _niaStatus = a })

instance FromXML NetworkInterfaceAttachment where
    parseXML x = NetworkInterfaceAttachment
        <$> x .@? "attachTime"
        <*> x .@? "attachmentId"
        <*> x .@? "deleteOnTermination"
        <*> x .@? "deviceIndex"
        <*> x .@? "instanceId"
        <*> x .@? "instanceOwnerId"
        <*> x .@? "status"

instance ToQuery NetworkInterfaceAttachment where
    toQuery NetworkInterfaceAttachment{..} = mconcat
        [ "AttachTime"          =? _niaAttachTime
        , "AttachmentId"        =? _niaAttachmentId
        , "DeleteOnTermination" =? _niaDeleteOnTermination
        , "DeviceIndex"         =? _niaDeviceIndex
        , "InstanceId"          =? _niaInstanceId
        , "InstanceOwnerId"     =? _niaInstanceOwnerId
        , "Status"              =? _niaStatus
        ]

newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { _rimeEnabled :: Bool
    } deriving (Eq, Ord, Read, Show, Enum)

-- | 'RunInstancesMonitoringEnabled' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimeEnabled' @::@ 'Bool'
--
runInstancesMonitoringEnabled :: Bool -- ^ 'rimeEnabled'
                              -> RunInstancesMonitoringEnabled
runInstancesMonitoringEnabled p1 = RunInstancesMonitoringEnabled
    { _rimeEnabled = p1
    }

-- | Indicates whether monitoring is enabled for the instance.
rimeEnabled :: Lens' RunInstancesMonitoringEnabled Bool
rimeEnabled = lens _rimeEnabled (\s a -> s { _rimeEnabled = a })

instance FromXML RunInstancesMonitoringEnabled where
    parseXML x = RunInstancesMonitoringEnabled
        <$> x .@  "enabled"

instance ToQuery RunInstancesMonitoringEnabled where
    toQuery RunInstancesMonitoringEnabled{..} = mconcat
        [ "Enabled" =? _rimeEnabled
        ]

data VolumeStatusInfo = VolumeStatusInfo
    { _vsiDetails :: List "item" VolumeStatusDetails
    , _vsiStatus  :: Maybe VolumeStatusInfoStatus
    } deriving (Eq, Read, Show)

-- | 'VolumeStatusInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsiDetails' @::@ ['VolumeStatusDetails']
--
-- * 'vsiStatus' @::@ 'Maybe' 'VolumeStatusInfoStatus'
--
volumeStatusInfo :: VolumeStatusInfo
volumeStatusInfo = VolumeStatusInfo
    { _vsiStatus  = Nothing
    , _vsiDetails = mempty
    }

-- | The details of the volume status.
vsiDetails :: Lens' VolumeStatusInfo [VolumeStatusDetails]
vsiDetails = lens _vsiDetails (\s a -> s { _vsiDetails = a }) . _List

-- | The status of the volume.
vsiStatus :: Lens' VolumeStatusInfo (Maybe VolumeStatusInfoStatus)
vsiStatus = lens _vsiStatus (\s a -> s { _vsiStatus = a })

instance FromXML VolumeStatusInfo where
    parseXML x = VolumeStatusInfo
        <$> x .@? "details" .!@ mempty
        <*> x .@? "status"

instance ToQuery VolumeStatusInfo where
    toQuery VolumeStatusInfo{..} = mconcat
        [ "Details" `toQueryList` _vsiDetails
        , "Status"  =? _vsiStatus
        ]

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { _niaAllocationId  :: Maybe Text
    , _niaAssociationId :: Maybe Text
    , _niaIpOwnerId     :: Maybe Text
    , _niaPublicDnsName :: Maybe Text
    , _niaPublicIp      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'NetworkInterfaceAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niaAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'niaAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'niaIpOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'niaPublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'niaPublicIp' @::@ 'Maybe' 'Text'
--
networkInterfaceAssociation :: NetworkInterfaceAssociation
networkInterfaceAssociation = NetworkInterfaceAssociation
    { _niaPublicIp      = Nothing
    , _niaPublicDnsName = Nothing
    , _niaIpOwnerId     = Nothing
    , _niaAllocationId  = Nothing
    , _niaAssociationId = Nothing
    }

-- | The allocation ID.
niaAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAllocationId = lens _niaAllocationId (\s a -> s { _niaAllocationId = a })

-- | The association ID.
niaAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAssociationId = lens _niaAssociationId (\s a -> s { _niaAssociationId = a })

-- | The ID of the Elastic IP address owner.
niaIpOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaIpOwnerId = lens _niaIpOwnerId (\s a -> s { _niaIpOwnerId = a })

-- | The public DNS name.
niaPublicDnsName :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicDnsName = lens _niaPublicDnsName (\s a -> s { _niaPublicDnsName = a })

-- | The address of the Elastic IP address bound to the network interface.
niaPublicIp :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicIp = lens _niaPublicIp (\s a -> s { _niaPublicIp = a })

instance FromXML NetworkInterfaceAssociation where
    parseXML x = NetworkInterfaceAssociation
        <$> x .@? "allocationId"
        <*> x .@? "associationId"
        <*> x .@? "ipOwnerId"
        <*> x .@? "publicDnsName"
        <*> x .@? "publicIp"

instance ToQuery NetworkInterfaceAssociation where
    toQuery NetworkInterfaceAssociation{..} = mconcat
        [ "AllocationId"  =? _niaAllocationId
        , "AssociationId" =? _niaAssociationId
        , "IpOwnerId"     =? _niaIpOwnerId
        , "PublicDnsName" =? _niaPublicDnsName
        , "PublicIp"      =? _niaPublicIp
        ]

data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd    :: List "item" CreateVolumePermission
    , _cvpmRemove :: List "item" CreateVolumePermission
    } deriving (Eq, Read, Show)

-- | 'CreateVolumePermissionModifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpmAdd' @::@ ['CreateVolumePermission']
--
-- * 'cvpmRemove' @::@ ['CreateVolumePermission']
--
createVolumePermissionModifications :: CreateVolumePermissionModifications
createVolumePermissionModifications = CreateVolumePermissionModifications
    { _cvpmAdd    = mempty
    , _cvpmRemove = mempty
    }

-- | Adds a specific AWS account ID or group to a volume's list of create volume
-- permissions.
cvpmAdd :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmAdd = lens _cvpmAdd (\s a -> s { _cvpmAdd = a }) . _List

-- | Removes a specific AWS account ID or group from a volume's list of create
-- volume permissions.
cvpmRemove :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmRemove = lens _cvpmRemove (\s a -> s { _cvpmRemove = a }) . _List

instance FromXML CreateVolumePermissionModifications where
    parseXML x = CreateVolumePermissionModifications
        <$> x .@? "Add" .!@ mempty
        <*> x .@? "Remove" .!@ mempty

instance ToQuery CreateVolumePermissionModifications where
    toQuery CreateVolumePermissionModifications{..} = mconcat
        [ "Add"    `toQueryList` _cvpmAdd
        , "Remove" `toQueryList` _cvpmRemove
        ]

data VpcState
    = VpcStateAvailable -- ^ available
    | VpcStatePending   -- ^ pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VpcState

instance FromText VpcState where
    parser = takeLowerText >>= \case
        "available" -> pure VpcStateAvailable
        "pending"   -> pure VpcStatePending
        e           -> fail $
            "Failure parsing VpcState from " ++ show e

instance ToText VpcState where
    toText = \case
        VpcStateAvailable -> "available"
        VpcStatePending   -> "pending"

instance ToByteString VpcState
instance ToHeader     VpcState
instance ToQuery      VpcState

instance FromXML VpcState where
    parseXML = parseXMLText "VpcState"

data ResourceType
    = RTCustomerGateway      -- ^ customer-gateway
    | RTDhcpOptions          -- ^ dhcp-options
    | RTImage                -- ^ image
    | RTInstance'            -- ^ instance
    | RTInternetGateway      -- ^ internet-gateway
    | RTNetworkAcl           -- ^ network-acl
    | RTNetworkInterface     -- ^ network-interface
    | RTReservedInstances    -- ^ reserved-instances
    | RTRouteTable           -- ^ route-table
    | RTSecurityGroup        -- ^ security-group
    | RTSnapshot             -- ^ snapshot
    | RTSpotInstancesRequest -- ^ spot-instances-request
    | RTSubnet               -- ^ subnet
    | RTVolume               -- ^ volume
    | RTVpc                  -- ^ vpc
    | RTVpnConnection        -- ^ vpn-connection
    | RTVpnGateway           -- ^ vpn-gateway
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ResourceType

instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "customer-gateway"       -> pure RTCustomerGateway
        "dhcp-options"           -> pure RTDhcpOptions
        "image"                  -> pure RTImage
        "instance"               -> pure RTInstance'
        "internet-gateway"       -> pure RTInternetGateway
        "network-acl"            -> pure RTNetworkAcl
        "network-interface"      -> pure RTNetworkInterface
        "reserved-instances"     -> pure RTReservedInstances
        "route-table"            -> pure RTRouteTable
        "security-group"         -> pure RTSecurityGroup
        "snapshot"               -> pure RTSnapshot
        "spot-instances-request" -> pure RTSpotInstancesRequest
        "subnet"                 -> pure RTSubnet
        "volume"                 -> pure RTVolume
        "vpc"                    -> pure RTVpc
        "vpn-connection"         -> pure RTVpnConnection
        "vpn-gateway"            -> pure RTVpnGateway
        e                        -> fail $
            "Failure parsing ResourceType from " ++ show e

instance ToText ResourceType where
    toText = \case
        RTCustomerGateway      -> "customer-gateway"
        RTDhcpOptions          -> "dhcp-options"
        RTImage                -> "image"
        RTInstance'            -> "instance"
        RTInternetGateway      -> "internet-gateway"
        RTNetworkAcl           -> "network-acl"
        RTNetworkInterface     -> "network-interface"
        RTReservedInstances    -> "reserved-instances"
        RTRouteTable           -> "route-table"
        RTSecurityGroup        -> "security-group"
        RTSnapshot             -> "snapshot"
        RTSpotInstancesRequest -> "spot-instances-request"
        RTSubnet               -> "subnet"
        RTVolume               -> "volume"
        RTVpc                  -> "vpc"
        RTVpnConnection        -> "vpn-connection"
        RTVpnGateway           -> "vpn-gateway"

instance ToByteString ResourceType
instance ToHeader     ResourceType
instance ToQuery      ResourceType

instance FromXML ResourceType where
    parseXML = parseXMLText "ResourceType"

data ReportStatusType
    = Impaired -- ^ impaired
    | Ok       -- ^ ok
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReportStatusType

instance FromText ReportStatusType where
    parser = takeLowerText >>= \case
        "impaired" -> pure Impaired
        "ok"       -> pure Ok
        e          -> fail $
            "Failure parsing ReportStatusType from " ++ show e

instance ToText ReportStatusType where
    toText = \case
        Impaired -> "impaired"
        Ok       -> "ok"

instance ToByteString ReportStatusType
instance ToHeader     ReportStatusType
instance ToQuery      ReportStatusType

instance FromXML ReportStatusType where
    parseXML = parseXMLText "ReportStatusType"

data CurrencyCodeValues
    = Usd -- ^ USD
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable CurrencyCodeValues

instance FromText CurrencyCodeValues where
    parser = takeLowerText >>= \case
        "usd" -> pure Usd
        e     -> fail $
            "Failure parsing CurrencyCodeValues from " ++ show e

instance ToText CurrencyCodeValues where
    toText Usd = "USD"

instance ToByteString CurrencyCodeValues
instance ToHeader     CurrencyCodeValues
instance ToQuery      CurrencyCodeValues

instance FromXML CurrencyCodeValues where
    parseXML = parseXMLText "CurrencyCodeValues"

data IcmpTypeCode = IcmpTypeCode
    { _itcCode :: Maybe Int
    , _itcType :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'IcmpTypeCode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'itcCode' @::@ 'Maybe' 'Int'
--
-- * 'itcType' @::@ 'Maybe' 'Int'
--
icmpTypeCode :: IcmpTypeCode
icmpTypeCode = IcmpTypeCode
    { _itcType = Nothing
    , _itcCode = Nothing
    }

-- | The ICMP type. A value of -1 means all types.
itcCode :: Lens' IcmpTypeCode (Maybe Int)
itcCode = lens _itcCode (\s a -> s { _itcCode = a })

-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
itcType :: Lens' IcmpTypeCode (Maybe Int)
itcType = lens _itcType (\s a -> s { _itcType = a })

instance FromXML IcmpTypeCode where
    parseXML x = IcmpTypeCode
        <$> x .@? "code"
        <*> x .@? "type"

instance ToQuery IcmpTypeCode where
    toQuery IcmpTypeCode{..} = mconcat
        [ "Code" =? _itcCode
        , "Type" =? _itcType
        ]

data InstanceCount = InstanceCount
    { _icInstanceCount :: Maybe Int
    , _icState         :: Maybe ListingState
    } deriving (Eq, Read, Show)

-- | 'InstanceCount' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'icState' @::@ 'Maybe' 'ListingState'
--
instanceCount :: InstanceCount
instanceCount = InstanceCount
    { _icState         = Nothing
    , _icInstanceCount = Nothing
    }

-- | The number of listed Reserved Instances in the state specified by the 'state'.
icInstanceCount :: Lens' InstanceCount (Maybe Int)
icInstanceCount = lens _icInstanceCount (\s a -> s { _icInstanceCount = a })

-- | The states of the listed Reserved Instances.
icState :: Lens' InstanceCount (Maybe ListingState)
icState = lens _icState (\s a -> s { _icState = a })

instance FromXML InstanceCount where
    parseXML x = InstanceCount
        <$> x .@? "instanceCount"
        <*> x .@? "state"

instance ToQuery InstanceCount where
    toQuery InstanceCount{..} = mconcat
        [ "InstanceCount" =? _icInstanceCount
        , "State"         =? _icState
        ]

data ExportToS3Task = ExportToS3Task
    { _etstContainerFormat :: Maybe ContainerFormat
    , _etstDiskImageFormat :: Maybe DiskImageFormat
    , _etstS3Bucket        :: Maybe Text
    , _etstS3Key           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ExportToS3Task' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etstContainerFormat' @::@ 'Maybe' 'ContainerFormat'
--
-- * 'etstDiskImageFormat' @::@ 'Maybe' 'DiskImageFormat'
--
-- * 'etstS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'etstS3Key' @::@ 'Maybe' 'Text'
--
exportToS3Task :: ExportToS3Task
exportToS3Task = ExportToS3Task
    { _etstDiskImageFormat = Nothing
    , _etstContainerFormat = Nothing
    , _etstS3Bucket        = Nothing
    , _etstS3Key           = Nothing
    }

-- | The container format used to combine disk images with metadata (such as OVF).
-- If absent, only the disk image is exported.
etstContainerFormat :: Lens' ExportToS3Task (Maybe ContainerFormat)
etstContainerFormat =
    lens _etstContainerFormat (\s a -> s { _etstContainerFormat = a })

-- | The format for the exported image.
etstDiskImageFormat :: Lens' ExportToS3Task (Maybe DiskImageFormat)
etstDiskImageFormat =
    lens _etstDiskImageFormat (\s a -> s { _etstDiskImageFormat = a })

-- | The S3 bucket for the destination image. The destination bucket must exist
-- and grant WRITE and READ_ACP permissions to the AWS account 'vm-import-export@amazon.com'.
etstS3Bucket :: Lens' ExportToS3Task (Maybe Text)
etstS3Bucket = lens _etstS3Bucket (\s a -> s { _etstS3Bucket = a })

-- | The encryption key for your S3 bucket.
etstS3Key :: Lens' ExportToS3Task (Maybe Text)
etstS3Key = lens _etstS3Key (\s a -> s { _etstS3Key = a })

instance FromXML ExportToS3Task where
    parseXML x = ExportToS3Task
        <$> x .@? "containerFormat"
        <*> x .@? "diskImageFormat"
        <*> x .@? "s3Bucket"
        <*> x .@? "s3Key"

instance ToQuery ExportToS3Task where
    toQuery ExportToS3Task{..} = mconcat
        [ "ContainerFormat" =? _etstContainerFormat
        , "DiskImageFormat" =? _etstDiskImageFormat
        , "S3Bucket"        =? _etstS3Bucket
        , "S3Key"           =? _etstS3Key
        ]

data PrefixList = PrefixList
    { _plCidrs          :: List "item" Text
    , _plPrefixListId   :: Maybe Text
    , _plPrefixListName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PrefixList' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'plCidrs' @::@ ['Text']
--
-- * 'plPrefixListId' @::@ 'Maybe' 'Text'
--
-- * 'plPrefixListName' @::@ 'Maybe' 'Text'
--
prefixList :: PrefixList
prefixList = PrefixList
    { _plPrefixListId   = Nothing
    , _plPrefixListName = Nothing
    , _plCidrs          = mempty
    }

-- | The IP address range of the AWS service.
plCidrs :: Lens' PrefixList [Text]
plCidrs = lens _plCidrs (\s a -> s { _plCidrs = a }) . _List

-- | The ID of the prefix.
plPrefixListId :: Lens' PrefixList (Maybe Text)
plPrefixListId = lens _plPrefixListId (\s a -> s { _plPrefixListId = a })

-- | The name of the prefix.
plPrefixListName :: Lens' PrefixList (Maybe Text)
plPrefixListName = lens _plPrefixListName (\s a -> s { _plPrefixListName = a })

instance FromXML PrefixList where
    parseXML x = PrefixList
        <$> x .@? "cidrSet" .!@ mempty
        <*> x .@? "prefixListId"
        <*> x .@? "prefixListName"

instance ToQuery PrefixList where
    toQuery PrefixList{..} = mconcat
        [ "CidrSet"        `toQueryList` _plCidrs
        , "PrefixListId"   =? _plPrefixListId
        , "PrefixListName" =? _plPrefixListName
        ]

data BlockDeviceMapping = BlockDeviceMapping
    { _bdmDeviceName  :: Text
    , _bdmEbs         :: Maybe EbsBlockDevice
    , _bdmNoDevice    :: Maybe Text
    , _bdmVirtualName :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'BlockDeviceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdmDeviceName' @::@ 'Text'
--
-- * 'bdmEbs' @::@ 'Maybe' 'EbsBlockDevice'
--
-- * 'bdmNoDevice' @::@ 'Maybe' 'Text'
--
-- * 'bdmVirtualName' @::@ 'Maybe' 'Text'
--
blockDeviceMapping :: Text -- ^ 'bdmDeviceName'
                   -> BlockDeviceMapping
blockDeviceMapping p1 = BlockDeviceMapping
    { _bdmDeviceName  = p1
    , _bdmVirtualName = Nothing
    , _bdmEbs         = Nothing
    , _bdmNoDevice    = Nothing
    }

-- | The device name exposed to the instance (for example, '/dev/sdh' or 'xvdh').
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\s a -> s { _bdmDeviceName = a })

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
bdmEbs :: Lens' BlockDeviceMapping (Maybe EbsBlockDevice)
bdmEbs = lens _bdmEbs (\s a -> s { _bdmEbs = a })

-- | Suppresses the specified device included in the block device mapping of the
-- AMI.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\s a -> s { _bdmNoDevice = a })

-- | The virtual device name ('ephemeral'N). Instance store volumes are numbered
-- starting from 0. An instance type with 2 available instance store volumes can
-- specify mappings for 'ephemeral0' and 'ephemeral1'.The number of available
-- instance store volumes depends on the instance type. After you connect to the
-- instance, you must mount the volume.
--
-- Constraints: For M3 instances, you must specify instance store volumes in
-- the block device mapping for the instance. When you launch an M3 instance, we
-- ignore any instance store volumes specified in the block device mapping for
-- the AMI.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s { _bdmVirtualName = a })

instance FromXML BlockDeviceMapping where
    parseXML x = BlockDeviceMapping
        <$> x .@  "deviceName"
        <*> x .@? "ebs"
        <*> x .@? "noDevice"
        <*> x .@? "virtualName"

instance ToQuery BlockDeviceMapping where
    toQuery BlockDeviceMapping{..} = mconcat
        [ "DeviceName"  =? _bdmDeviceName
        , "Ebs"         =? _bdmEbs
        , "NoDevice"    =? _bdmNoDevice
        , "VirtualName" =? _bdmVirtualName
        ]

data ConversionTask = ConversionTask
    { _ctConversionTaskId :: Text
    , _ctExpirationTime   :: Maybe Text
    , _ctImportInstance   :: Maybe ImportInstanceTaskDetails
    , _ctImportVolume     :: Maybe ImportVolumeTaskDetails
    , _ctState            :: ConversionTaskState
    , _ctStatusMessage    :: Maybe Text
    , _ctTags             :: List "item" Tag
    } deriving (Eq, Read, Show)

-- | 'ConversionTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctConversionTaskId' @::@ 'Text'
--
-- * 'ctExpirationTime' @::@ 'Maybe' 'Text'
--
-- * 'ctImportInstance' @::@ 'Maybe' 'ImportInstanceTaskDetails'
--
-- * 'ctImportVolume' @::@ 'Maybe' 'ImportVolumeTaskDetails'
--
-- * 'ctState' @::@ 'ConversionTaskState'
--
-- * 'ctStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'ctTags' @::@ ['Tag']
--
conversionTask :: Text -- ^ 'ctConversionTaskId'
               -> ConversionTaskState -- ^ 'ctState'
               -> ConversionTask
conversionTask p1 p2 = ConversionTask
    { _ctConversionTaskId = p1
    , _ctState            = p2
    , _ctExpirationTime   = Nothing
    , _ctImportInstance   = Nothing
    , _ctImportVolume     = Nothing
    , _ctStatusMessage    = Nothing
    , _ctTags             = mempty
    }

-- | The ID of the conversion task.
ctConversionTaskId :: Lens' ConversionTask Text
ctConversionTaskId =
    lens _ctConversionTaskId (\s a -> s { _ctConversionTaskId = a })

-- | The time when the task expires. If the upload isn't complete before the
-- expiration time, we automatically cancel the task.
ctExpirationTime :: Lens' ConversionTask (Maybe Text)
ctExpirationTime = lens _ctExpirationTime (\s a -> s { _ctExpirationTime = a })

-- | If the task is for importing an instance, this contains information about the
-- import instance task.
ctImportInstance :: Lens' ConversionTask (Maybe ImportInstanceTaskDetails)
ctImportInstance = lens _ctImportInstance (\s a -> s { _ctImportInstance = a })

-- | If the task is for importing a volume, this contains information about the
-- import volume task.
ctImportVolume :: Lens' ConversionTask (Maybe ImportVolumeTaskDetails)
ctImportVolume = lens _ctImportVolume (\s a -> s { _ctImportVolume = a })

-- | The state of the conversion task.
ctState :: Lens' ConversionTask ConversionTaskState
ctState = lens _ctState (\s a -> s { _ctState = a })

-- | The status message related to the conversion task.
ctStatusMessage :: Lens' ConversionTask (Maybe Text)
ctStatusMessage = lens _ctStatusMessage (\s a -> s { _ctStatusMessage = a })

-- | Any tags assigned to the task.
ctTags :: Lens' ConversionTask [Tag]
ctTags = lens _ctTags (\s a -> s { _ctTags = a }) . _List

instance FromXML ConversionTask where
    parseXML x = ConversionTask
        <$> x .@  "conversionTaskId"
        <*> x .@? "expirationTime"
        <*> x .@? "importInstance"
        <*> x .@? "importVolume"
        <*> x .@  "state"
        <*> x .@? "statusMessage"
        <*> x .@? "tagSet" .!@ mempty

instance ToQuery ConversionTask where
    toQuery ConversionTask{..} = mconcat
        [ "ConversionTaskId" =? _ctConversionTaskId
        , "ExpirationTime"   =? _ctExpirationTime
        , "ImportInstance"   =? _ctImportInstance
        , "ImportVolume"     =? _ctImportVolume
        , "State"            =? _ctState
        , "StatusMessage"    =? _ctStatusMessage
        , "TagSet"           `toQueryList` _ctTags
        ]

data AttachmentStatus
    = Attached  -- ^ attached
    | Attaching -- ^ attaching
    | Detached  -- ^ detached
    | Detaching -- ^ detaching
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AttachmentStatus

instance FromText AttachmentStatus where
    parser = takeLowerText >>= \case
        "attached"  -> pure Attached
        "attaching" -> pure Attaching
        "detached"  -> pure Detached
        "detaching" -> pure Detaching
        e           -> fail $
            "Failure parsing AttachmentStatus from " ++ show e

instance ToText AttachmentStatus where
    toText = \case
        Attached  -> "attached"
        Attaching -> "attaching"
        Detached  -> "detached"
        Detaching -> "detaching"

instance ToByteString AttachmentStatus
instance ToHeader     AttachmentStatus
instance ToQuery      AttachmentStatus

instance FromXML AttachmentStatus where
    parseXML = parseXMLText "AttachmentStatus"

data ClassicLinkInstance = ClassicLinkInstance
    { _cliGroups     :: List "item" GroupIdentifier
    , _cliInstanceId :: Maybe Text
    , _cliTags       :: List "item" Tag
    , _cliVpcId      :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ClassicLinkInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cliGroups' @::@ ['GroupIdentifier']
--
-- * 'cliInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'cliTags' @::@ ['Tag']
--
-- * 'cliVpcId' @::@ 'Maybe' 'Text'
--
classicLinkInstance :: ClassicLinkInstance
classicLinkInstance = ClassicLinkInstance
    { _cliInstanceId = Nothing
    , _cliVpcId      = Nothing
    , _cliGroups     = mempty
    , _cliTags       = mempty
    }

-- | A list of security groups.
cliGroups :: Lens' ClassicLinkInstance [GroupIdentifier]
cliGroups = lens _cliGroups (\s a -> s { _cliGroups = a }) . _List

-- | The ID of the instance.
cliInstanceId :: Lens' ClassicLinkInstance (Maybe Text)
cliInstanceId = lens _cliInstanceId (\s a -> s { _cliInstanceId = a })

-- | Any tags assigned to the instance.
cliTags :: Lens' ClassicLinkInstance [Tag]
cliTags = lens _cliTags (\s a -> s { _cliTags = a }) . _List

-- | The ID of the VPC.
cliVpcId :: Lens' ClassicLinkInstance (Maybe Text)
cliVpcId = lens _cliVpcId (\s a -> s { _cliVpcId = a })

instance FromXML ClassicLinkInstance where
    parseXML x = ClassicLinkInstance
        <$> x .@? "groupSet" .!@ mempty
        <*> x .@? "instanceId"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "vpcId"

instance ToQuery ClassicLinkInstance where
    toQuery ClassicLinkInstance{..} = mconcat
        [ "GroupSet"   `toQueryList` _cliGroups
        , "InstanceId" =? _cliInstanceId
        , "TagSet"     `toQueryList` _cliTags
        , "VpcId"      =? _cliVpcId
        ]

data RouteOrigin
    = OriginCreateRoute               -- ^ CreateRoute
    | OriginCreateRouteTable          -- ^ CreateRouteTable
    | OriginEnableVgwRoutePropagation -- ^ EnableVgwRoutePropagation
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable RouteOrigin

instance FromText RouteOrigin where
    parser = takeLowerText >>= \case
        "createroute"               -> pure OriginCreateRoute
        "createroutetable"          -> pure OriginCreateRouteTable
        "enablevgwroutepropagation" -> pure OriginEnableVgwRoutePropagation
        e                           -> fail $
            "Failure parsing RouteOrigin from " ++ show e

instance ToText RouteOrigin where
    toText = \case
        OriginCreateRoute               -> "CreateRoute"
        OriginCreateRouteTable          -> "CreateRouteTable"
        OriginEnableVgwRoutePropagation -> "EnableVgwRoutePropagation"

instance ToByteString RouteOrigin
instance ToHeader     RouteOrigin
instance ToQuery      RouteOrigin

instance FromXML RouteOrigin where
    parseXML = parseXMLText "RouteOrigin"

data ListingState
    = LSAvailable -- ^ available
    | LSCancelled -- ^ cancelled
    | LSPending   -- ^ pending
    | LSSold      -- ^ sold
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ListingState

instance FromText ListingState where
    parser = takeLowerText >>= \case
        "available" -> pure LSAvailable
        "cancelled" -> pure LSCancelled
        "pending"   -> pure LSPending
        "sold"      -> pure LSSold
        e           -> fail $
            "Failure parsing ListingState from " ++ show e

instance ToText ListingState where
    toText = \case
        LSAvailable -> "available"
        LSCancelled -> "cancelled"
        LSPending   -> "pending"
        LSSold      -> "sold"

instance ToByteString ListingState
instance ToHeader     ListingState
instance ToQuery      ListingState

instance FromXML ListingState where
    parseXML = parseXMLText "ListingState"

data SpotPrice = SpotPrice
    { _spAvailabilityZone   :: Maybe Text
    , _spInstanceType       :: Maybe InstanceType
    , _spProductDescription :: Maybe RIProductDescription
    , _spSpotPrice          :: Maybe Text
    , _spTimestamp          :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'SpotPrice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'spInstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'spProductDescription' @::@ 'Maybe' 'RIProductDescription'
--
-- * 'spSpotPrice' @::@ 'Maybe' 'Text'
--
-- * 'spTimestamp' @::@ 'Maybe' 'UTCTime'
--
spotPrice :: SpotPrice
spotPrice = SpotPrice
    { _spInstanceType       = Nothing
    , _spProductDescription = Nothing
    , _spSpotPrice          = Nothing
    , _spTimestamp          = Nothing
    , _spAvailabilityZone   = Nothing
    }

-- | The Availability Zone.
spAvailabilityZone :: Lens' SpotPrice (Maybe Text)
spAvailabilityZone =
    lens _spAvailabilityZone (\s a -> s { _spAvailabilityZone = a })

-- | The instance type.
spInstanceType :: Lens' SpotPrice (Maybe InstanceType)
spInstanceType = lens _spInstanceType (\s a -> s { _spInstanceType = a })

-- | A general description of the AMI.
spProductDescription :: Lens' SpotPrice (Maybe RIProductDescription)
spProductDescription =
    lens _spProductDescription (\s a -> s { _spProductDescription = a })

-- | The maximum price (bid) that you are willing to pay for a Spot Instance.
spSpotPrice :: Lens' SpotPrice (Maybe Text)
spSpotPrice = lens _spSpotPrice (\s a -> s { _spSpotPrice = a })

-- | The date and time the request was created, in UTC format (for example, /YYYY/-/MM/
-- -/DD/T/HH/:/MM/:/SS/Z).
spTimestamp :: Lens' SpotPrice (Maybe UTCTime)
spTimestamp = lens _spTimestamp (\s a -> s { _spTimestamp = a }) . mapping _Time

instance FromXML SpotPrice where
    parseXML x = SpotPrice
        <$> x .@? "availabilityZone"
        <*> x .@? "instanceType"
        <*> x .@? "productDescription"
        <*> x .@? "spotPrice"
        <*> x .@? "timestamp"

instance ToQuery SpotPrice where
    toQuery SpotPrice{..} = mconcat
        [ "AvailabilityZone"   =? _spAvailabilityZone
        , "InstanceType"       =? _spInstanceType
        , "ProductDescription" =? _spProductDescription
        , "SpotPrice"          =? _spSpotPrice
        , "Timestamp"          =? _spTimestamp
        ]

data ActiveInstance = ActiveInstance
    { _aiInstanceId            :: Maybe Text
    , _aiInstanceType          :: Maybe Text
    , _aiSpotInstanceRequestId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ActiveInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'aiInstanceType' @::@ 'Maybe' 'Text'
--
-- * 'aiSpotInstanceRequestId' @::@ 'Maybe' 'Text'
--
activeInstance :: ActiveInstance
activeInstance = ActiveInstance
    { _aiInstanceType          = Nothing
    , _aiInstanceId            = Nothing
    , _aiSpotInstanceRequestId = Nothing
    }

-- | The ID of the instance.
aiInstanceId :: Lens' ActiveInstance (Maybe Text)
aiInstanceId = lens _aiInstanceId (\s a -> s { _aiInstanceId = a })

-- | The instance type.
aiInstanceType :: Lens' ActiveInstance (Maybe Text)
aiInstanceType = lens _aiInstanceType (\s a -> s { _aiInstanceType = a })

-- | The ID of the Spot Instance request.
aiSpotInstanceRequestId :: Lens' ActiveInstance (Maybe Text)
aiSpotInstanceRequestId =
    lens _aiSpotInstanceRequestId (\s a -> s { _aiSpotInstanceRequestId = a })

instance FromXML ActiveInstance where
    parseXML x = ActiveInstance
        <$> x .@? "instanceId"
        <*> x .@? "instanceType"
        <*> x .@? "spotInstanceRequestId"

instance ToQuery ActiveInstance where
    toQuery ActiveInstance{..} = mconcat
        [ "InstanceId"            =? _aiInstanceId
        , "InstanceType"          =? _aiInstanceType
        , "SpotInstanceRequestId" =? _aiSpotInstanceRequestId
        ]

data SpotFleetRequestConfigData = SpotFleetRequestConfigData
    { _sfrcdClientToken                      :: Maybe Text
    , _sfrcdIamFleetRole                     :: Text
    , _sfrcdLaunchSpecifications             :: List1 "item" LaunchSpecification
    , _sfrcdSpotPrice                        :: Text
    , _sfrcdTargetCapacity                   :: Int
    , _sfrcdTerminateInstancesWithExpiration :: Maybe Bool
    , _sfrcdValidFrom                        :: Maybe ISO8601
    , _sfrcdValidUntil                       :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'SpotFleetRequestConfigData' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sfrcdClientToken' @::@ 'Maybe' 'Text'
--
-- * 'sfrcdIamFleetRole' @::@ 'Text'
--
-- * 'sfrcdLaunchSpecifications' @::@ 'NonEmpty' 'LaunchSpecification'
--
-- * 'sfrcdSpotPrice' @::@ 'Text'
--
-- * 'sfrcdTargetCapacity' @::@ 'Int'
--
-- * 'sfrcdTerminateInstancesWithExpiration' @::@ 'Maybe' 'Bool'
--
-- * 'sfrcdValidFrom' @::@ 'Maybe' 'UTCTime'
--
-- * 'sfrcdValidUntil' @::@ 'Maybe' 'UTCTime'
--
spotFleetRequestConfigData :: Text -- ^ 'sfrcdSpotPrice'
                           -> Int -- ^ 'sfrcdTargetCapacity'
                           -> Text -- ^ 'sfrcdIamFleetRole'
                           -> NonEmpty LaunchSpecification -- ^ 'sfrcdLaunchSpecifications'
                           -> SpotFleetRequestConfigData
spotFleetRequestConfigData p1 p2 p3 p4 = SpotFleetRequestConfigData
    { _sfrcdSpotPrice                        = p1
    , _sfrcdTargetCapacity                   = p2
    , _sfrcdIamFleetRole                     = p3
    , _sfrcdLaunchSpecifications             = withIso _List1 (const id) p4
    , _sfrcdClientToken                      = Nothing
    , _sfrcdValidFrom                        = Nothing
    , _sfrcdValidUntil                       = Nothing
    , _sfrcdTerminateInstancesWithExpiration = Nothing
    }

-- | A unique, case-sensitive identifier you provide to ensure idempotency of your
-- listings. This helps avoid duplicate listings. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
sfrcdClientToken :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdClientToken = lens _sfrcdClientToken (\s a -> s { _sfrcdClientToken = a })

-- | Grants the Spot fleet service permission to terminate instances on your
-- behalf when you cancel a Spot fleet request using 'CancelSpotFleetRequests' or
-- when the Spot fleet request expires, if you set 'terminateInstancesWithExpiration'.
sfrcdIamFleetRole :: Lens' SpotFleetRequestConfigData Text
sfrcdIamFleetRole =
    lens _sfrcdIamFleetRole (\s a -> s { _sfrcdIamFleetRole = a })

-- | Information about the launch specifications for the instances.
sfrcdLaunchSpecifications :: Lens' SpotFleetRequestConfigData (NonEmpty LaunchSpecification)
sfrcdLaunchSpecifications =
    lens _sfrcdLaunchSpecifications
        (\s a -> s { _sfrcdLaunchSpecifications = a })
            . _List1

-- | The maximum hourly price (bid) for any Spot Instance launched to fulfill the
-- request.
sfrcdSpotPrice :: Lens' SpotFleetRequestConfigData Text
sfrcdSpotPrice = lens _sfrcdSpotPrice (\s a -> s { _sfrcdSpotPrice = a })

-- | The maximum number of Spot Instances to launch.
sfrcdTargetCapacity :: Lens' SpotFleetRequestConfigData Int
sfrcdTargetCapacity =
    lens _sfrcdTargetCapacity (\s a -> s { _sfrcdTargetCapacity = a })

-- | Indicates whether running instances should be terminated when the Spot fleet
-- request expires.
sfrcdTerminateInstancesWithExpiration :: Lens' SpotFleetRequestConfigData (Maybe Bool)
sfrcdTerminateInstancesWithExpiration =
    lens _sfrcdTerminateInstancesWithExpiration
        (\s a -> s { _sfrcdTerminateInstancesWithExpiration = a })

-- | The start date and time of the request, in UTC format (for example, /YYYY/-/MM/-/DD/
-- T/HH/:/MM/:/SS/Z). The default is to start fulfilling the request immediately.
sfrcdValidFrom :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidFrom = lens _sfrcdValidFrom (\s a -> s { _sfrcdValidFrom = a }) . mapping _Time

-- | The end date and time of the request, in UTC format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new Spot Instance requests are placed or enabled
-- to fulfill the request.
sfrcdValidUntil :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidUntil = lens _sfrcdValidUntil (\s a -> s { _sfrcdValidUntil = a }) . mapping _Time

instance FromXML SpotFleetRequestConfigData where
    parseXML x = SpotFleetRequestConfigData
        <$> x .@? "clientToken"
        <*> x .@  "iamFleetRole"
        <*> x .@  "launchSpecifications"
        <*> x .@  "spotPrice"
        <*> x .@  "targetCapacity"
        <*> x .@? "terminateInstancesWithExpiration"
        <*> x .@? "validFrom"
        <*> x .@? "validUntil"

instance ToQuery SpotFleetRequestConfigData where
    toQuery SpotFleetRequestConfigData{..} = mconcat
        [ "ClientToken"                      =? _sfrcdClientToken
        , "IamFleetRole"                     =? _sfrcdIamFleetRole
        , "LaunchSpecifications"             =? _sfrcdLaunchSpecifications
        , "SpotPrice"                        =? _sfrcdSpotPrice
        , "TargetCapacity"                   =? _sfrcdTargetCapacity
        , "TerminateInstancesWithExpiration" =? _sfrcdTerminateInstancesWithExpiration
        , "ValidFrom"                        =? _sfrcdValidFrom
        , "ValidUntil"                       =? _sfrcdValidUntil
        ]

data InstanceMonitoring = InstanceMonitoring
    { _imInstanceId :: Maybe Text
    , _imMonitoring :: Maybe Monitoring
    } deriving (Eq, Read, Show)

-- | 'InstanceMonitoring' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'imMonitoring' @::@ 'Maybe' 'Monitoring'
--
instanceMonitoring :: InstanceMonitoring
instanceMonitoring = InstanceMonitoring
    { _imInstanceId = Nothing
    , _imMonitoring = Nothing
    }

-- | The ID of the instance.
imInstanceId :: Lens' InstanceMonitoring (Maybe Text)
imInstanceId = lens _imInstanceId (\s a -> s { _imInstanceId = a })

-- | The monitoring information.
imMonitoring :: Lens' InstanceMonitoring (Maybe Monitoring)
imMonitoring = lens _imMonitoring (\s a -> s { _imMonitoring = a })

instance FromXML InstanceMonitoring where
    parseXML x = InstanceMonitoring
        <$> x .@? "instanceId"
        <*> x .@? "monitoring"

instance ToQuery InstanceMonitoring where
    toQuery InstanceMonitoring{..} = mconcat
        [ "InstanceId" =? _imInstanceId
        , "Monitoring" =? _imMonitoring
        ]

data PriceScheduleSpecification = PriceScheduleSpecification
    { _pssCurrencyCode :: Maybe CurrencyCodeValues
    , _pssPrice        :: Maybe Double
    , _pssTerm         :: Maybe Integer
    } deriving (Eq, Read, Show)

-- | 'PriceScheduleSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pssCurrencyCode' @::@ 'Maybe' 'CurrencyCodeValues'
--
-- * 'pssPrice' @::@ 'Maybe' 'Double'
--
-- * 'pssTerm' @::@ 'Maybe' 'Integer'
--
priceScheduleSpecification :: PriceScheduleSpecification
priceScheduleSpecification = PriceScheduleSpecification
    { _pssTerm         = Nothing
    , _pssPrice        = Nothing
    , _pssCurrencyCode = Nothing
    }

-- | The currency for transacting the Reserved Instance resale. At this time, the
-- only supported currency is 'USD'.
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe CurrencyCodeValues)
pssCurrencyCode = lens _pssCurrencyCode (\s a -> s { _pssCurrencyCode = a })

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice = lens _pssPrice (\s a -> s { _pssPrice = a })

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm = lens _pssTerm (\s a -> s { _pssTerm = a })

instance FromXML PriceScheduleSpecification where
    parseXML x = PriceScheduleSpecification
        <$> x .@? "currencyCode"
        <*> x .@? "price"
        <*> x .@? "term"

instance ToQuery PriceScheduleSpecification where
    toQuery PriceScheduleSpecification{..} = mconcat
        [ "CurrencyCode" =? _pssCurrencyCode
        , "Price"        =? _pssPrice
        , "Term"         =? _pssTerm
        ]

data SpotFleetRequestConfig = SpotFleetRequestConfig
    { _sfrcSpotFleetRequestConfig :: SpotFleetRequestConfigData
    , _sfrcSpotFleetRequestId     :: Text
    , _sfrcSpotFleetRequestState  :: BatchState
    } deriving (Eq, Read, Show)

-- | 'SpotFleetRequestConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sfrcSpotFleetRequestConfig' @::@ 'SpotFleetRequestConfigData'
--
-- * 'sfrcSpotFleetRequestId' @::@ 'Text'
--
-- * 'sfrcSpotFleetRequestState' @::@ 'BatchState'
--
spotFleetRequestConfig :: Text -- ^ 'sfrcSpotFleetRequestId'
                       -> BatchState -- ^ 'sfrcSpotFleetRequestState'
                       -> SpotFleetRequestConfigData -- ^ 'sfrcSpotFleetRequestConfig'
                       -> SpotFleetRequestConfig
spotFleetRequestConfig p1 p2 p3 = SpotFleetRequestConfig
    { _sfrcSpotFleetRequestId     = p1
    , _sfrcSpotFleetRequestState  = p2
    , _sfrcSpotFleetRequestConfig = p3
    }

-- | Information about the configuration of the Spot fleet request.
sfrcSpotFleetRequestConfig :: Lens' SpotFleetRequestConfig SpotFleetRequestConfigData
sfrcSpotFleetRequestConfig =
    lens _sfrcSpotFleetRequestConfig
        (\s a -> s { _sfrcSpotFleetRequestConfig = a })

-- | The ID of the Spot fleet request.
sfrcSpotFleetRequestId :: Lens' SpotFleetRequestConfig Text
sfrcSpotFleetRequestId =
    lens _sfrcSpotFleetRequestId (\s a -> s { _sfrcSpotFleetRequestId = a })

-- | The state of the Spot fleet request.
sfrcSpotFleetRequestState :: Lens' SpotFleetRequestConfig BatchState
sfrcSpotFleetRequestState =
    lens _sfrcSpotFleetRequestState
        (\s a -> s { _sfrcSpotFleetRequestState = a })

instance FromXML SpotFleetRequestConfig where
    parseXML x = SpotFleetRequestConfig
        <$> x .@  "spotFleetRequestConfig"
        <*> x .@  "spotFleetRequestId"
        <*> x .@  "spotFleetRequestState"

instance ToQuery SpotFleetRequestConfig where
    toQuery SpotFleetRequestConfig{..} = mconcat
        [ "SpotFleetRequestConfig" =? _sfrcSpotFleetRequestConfig
        , "SpotFleetRequestId"     =? _sfrcSpotFleetRequestId
        , "SpotFleetRequestState"  =? _sfrcSpotFleetRequestState
        ]

data SpotInstanceStatus = SpotInstanceStatus
    { _sisCode       :: Maybe Text
    , _sisMessage    :: Maybe Text
    , _sisUpdateTime :: Maybe ISO8601
    } deriving (Eq, Ord, Read, Show)

-- | 'SpotInstanceStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sisCode' @::@ 'Maybe' 'Text'
--
-- * 'sisMessage' @::@ 'Maybe' 'Text'
--
-- * 'sisUpdateTime' @::@ 'Maybe' 'UTCTime'
--
spotInstanceStatus :: SpotInstanceStatus
spotInstanceStatus = SpotInstanceStatus
    { _sisCode       = Nothing
    , _sisUpdateTime = Nothing
    , _sisMessage    = Nothing
    }

-- | The status code.
sisCode :: Lens' SpotInstanceStatus (Maybe Text)
sisCode = lens _sisCode (\s a -> s { _sisCode = a })

-- | The description for the status code.
sisMessage :: Lens' SpotInstanceStatus (Maybe Text)
sisMessage = lens _sisMessage (\s a -> s { _sisMessage = a })

-- | The date and time of the most recent status update, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
sisUpdateTime :: Lens' SpotInstanceStatus (Maybe UTCTime)
sisUpdateTime = lens _sisUpdateTime (\s a -> s { _sisUpdateTime = a }) . mapping _Time

instance FromXML SpotInstanceStatus where
    parseXML x = SpotInstanceStatus
        <$> x .@? "code"
        <*> x .@? "message"
        <*> x .@? "updateTime"

instance ToQuery SpotInstanceStatus where
    toQuery SpotInstanceStatus{..} = mconcat
        [ "Code"       =? _sisCode
        , "Message"    =? _sisMessage
        , "UpdateTime" =? _sisUpdateTime
        ]

data SnapshotTaskDetail = SnapshotTaskDetail
    { _stdDescription   :: Maybe Text
    , _stdDiskImageSize :: Maybe Double
    , _stdFormat        :: Maybe Text
    , _stdProgress      :: Maybe Text
    , _stdSnapshotId    :: Maybe Text
    , _stdStatus        :: Maybe Text
    , _stdStatusMessage :: Maybe Text
    , _stdUrl           :: Maybe Text
    , _stdUserBucket    :: Maybe UserBucketDetails
    } deriving (Eq, Read, Show)

-- | 'SnapshotTaskDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdDescription' @::@ 'Maybe' 'Text'
--
-- * 'stdDiskImageSize' @::@ 'Maybe' 'Double'
--
-- * 'stdFormat' @::@ 'Maybe' 'Text'
--
-- * 'stdProgress' @::@ 'Maybe' 'Text'
--
-- * 'stdSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'stdStatus' @::@ 'Maybe' 'Text'
--
-- * 'stdStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'stdUrl' @::@ 'Maybe' 'Text'
--
-- * 'stdUserBucket' @::@ 'Maybe' 'UserBucketDetails'
--
snapshotTaskDetail :: SnapshotTaskDetail
snapshotTaskDetail = SnapshotTaskDetail
    { _stdDiskImageSize = Nothing
    , _stdDescription   = Nothing
    , _stdFormat        = Nothing
    , _stdUrl           = Nothing
    , _stdUserBucket    = Nothing
    , _stdSnapshotId    = Nothing
    , _stdProgress      = Nothing
    , _stdStatusMessage = Nothing
    , _stdStatus        = Nothing
    }

-- | The description of the snapshot.
stdDescription :: Lens' SnapshotTaskDetail (Maybe Text)
stdDescription = lens _stdDescription (\s a -> s { _stdDescription = a })

-- | The size of the disk in the snapshot, in GiB.
stdDiskImageSize :: Lens' SnapshotTaskDetail (Maybe Double)
stdDiskImageSize = lens _stdDiskImageSize (\s a -> s { _stdDiskImageSize = a })

-- | The format of the disk image from which the snapshot is created.
stdFormat :: Lens' SnapshotTaskDetail (Maybe Text)
stdFormat = lens _stdFormat (\s a -> s { _stdFormat = a })

-- | The percentage of completion for the import snapshot task.
stdProgress :: Lens' SnapshotTaskDetail (Maybe Text)
stdProgress = lens _stdProgress (\s a -> s { _stdProgress = a })

-- | The snapshot ID of the disk being imported.
stdSnapshotId :: Lens' SnapshotTaskDetail (Maybe Text)
stdSnapshotId = lens _stdSnapshotId (\s a -> s { _stdSnapshotId = a })

-- | A brief status for the import snapshot task.
stdStatus :: Lens' SnapshotTaskDetail (Maybe Text)
stdStatus = lens _stdStatus (\s a -> s { _stdStatus = a })

-- | A detailed status message for the import snapshot task.
stdStatusMessage :: Lens' SnapshotTaskDetail (Maybe Text)
stdStatusMessage = lens _stdStatusMessage (\s a -> s { _stdStatusMessage = a })

-- | The URL of the disk image from which the snapshot is created.
stdUrl :: Lens' SnapshotTaskDetail (Maybe Text)
stdUrl = lens _stdUrl (\s a -> s { _stdUrl = a })

-- | The S3 bucket for the disk image.
stdUserBucket :: Lens' SnapshotTaskDetail (Maybe UserBucketDetails)
stdUserBucket = lens _stdUserBucket (\s a -> s { _stdUserBucket = a })

instance FromXML SnapshotTaskDetail where
    parseXML x = SnapshotTaskDetail
        <$> x .@? "description"
        <*> x .@? "diskImageSize"
        <*> x .@? "format"
        <*> x .@? "progress"
        <*> x .@? "snapshotId"
        <*> x .@? "status"
        <*> x .@? "statusMessage"
        <*> x .@? "url"
        <*> x .@? "userBucket"

instance ToQuery SnapshotTaskDetail where
    toQuery SnapshotTaskDetail{..} = mconcat
        [ "Description"   =? _stdDescription
        , "DiskImageSize" =? _stdDiskImageSize
        , "Format"        =? _stdFormat
        , "Progress"      =? _stdProgress
        , "SnapshotId"    =? _stdSnapshotId
        , "Status"        =? _stdStatus
        , "StatusMessage" =? _stdStatusMessage
        , "Url"           =? _stdUrl
        , "UserBucket"    =? _stdUserBucket
        ]

data AvailabilityZoneState
    = AZSAvailable -- ^ available
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AvailabilityZoneState

instance FromText AvailabilityZoneState where
    parser = takeLowerText >>= \case
        "available" -> pure AZSAvailable
        e           -> fail $
            "Failure parsing AvailabilityZoneState from " ++ show e

instance ToText AvailabilityZoneState where
    toText AZSAvailable = "available"

instance ToByteString AvailabilityZoneState
instance ToHeader     AvailabilityZoneState
instance ToQuery      AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    parseXML = parseXMLText "AvailabilityZoneState"

data SpotInstanceRequest = SpotInstanceRequest
    { _siAvailabilityZoneGroup    :: Maybe Text
    , _siCreateTime               :: Maybe ISO8601
    , _siFault                    :: Maybe SpotInstanceStateFault
    , _siInstanceId               :: Maybe Text
    , _siLaunchGroup              :: Maybe Text
    , _siLaunchSpecification      :: Maybe LaunchSpecification
    , _siLaunchedAvailabilityZone :: Maybe Text
    , _siProductDescription       :: Maybe RIProductDescription
    , _siSpotInstanceRequestId    :: Maybe Text
    , _siSpotPrice                :: Maybe Text
    , _siState                    :: Maybe SpotInstanceState
    , _siStatus                   :: Maybe SpotInstanceStatus
    , _siTags                     :: List "item" Tag
    , _siType                     :: Maybe SpotInstanceType
    , _siValidFrom                :: Maybe ISO8601
    , _siValidUntil               :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'SpotInstanceRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siAvailabilityZoneGroup' @::@ 'Maybe' 'Text'
--
-- * 'siCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'siFault' @::@ 'Maybe' 'SpotInstanceStateFault'
--
-- * 'siInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'siLaunchGroup' @::@ 'Maybe' 'Text'
--
-- * 'siLaunchSpecification' @::@ 'Maybe' 'LaunchSpecification'
--
-- * 'siLaunchedAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'siProductDescription' @::@ 'Maybe' 'RIProductDescription'
--
-- * 'siSpotInstanceRequestId' @::@ 'Maybe' 'Text'
--
-- * 'siSpotPrice' @::@ 'Maybe' 'Text'
--
-- * 'siState' @::@ 'Maybe' 'SpotInstanceState'
--
-- * 'siStatus' @::@ 'Maybe' 'SpotInstanceStatus'
--
-- * 'siTags' @::@ ['Tag']
--
-- * 'siType' @::@ 'Maybe' 'SpotInstanceType'
--
-- * 'siValidFrom' @::@ 'Maybe' 'UTCTime'
--
-- * 'siValidUntil' @::@ 'Maybe' 'UTCTime'
--
spotInstanceRequest :: SpotInstanceRequest
spotInstanceRequest = SpotInstanceRequest
    { _siSpotInstanceRequestId    = Nothing
    , _siSpotPrice                = Nothing
    , _siType                     = Nothing
    , _siState                    = Nothing
    , _siFault                    = Nothing
    , _siStatus                   = Nothing
    , _siValidFrom                = Nothing
    , _siValidUntil               = Nothing
    , _siLaunchGroup              = Nothing
    , _siAvailabilityZoneGroup    = Nothing
    , _siLaunchSpecification      = Nothing
    , _siInstanceId               = Nothing
    , _siCreateTime               = Nothing
    , _siProductDescription       = Nothing
    , _siTags                     = mempty
    , _siLaunchedAvailabilityZone = Nothing
    }

-- | The Availability Zone group. If you specify the same Availability Zone group
-- for all Spot Instance requests, all Spot Instances are launched in the same
-- Availability Zone.
siAvailabilityZoneGroup :: Lens' SpotInstanceRequest (Maybe Text)
siAvailabilityZoneGroup =
    lens _siAvailabilityZoneGroup (\s a -> s { _siAvailabilityZoneGroup = a })

-- | The date and time when the Spot Instance request was created, in UTC format
-- (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
siCreateTime :: Lens' SpotInstanceRequest (Maybe UTCTime)
siCreateTime = lens _siCreateTime (\s a -> s { _siCreateTime = a }) . mapping _Time

-- | The fault codes for the Spot Instance request, if any.
siFault :: Lens' SpotInstanceRequest (Maybe SpotInstanceStateFault)
siFault = lens _siFault (\s a -> s { _siFault = a })

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
siInstanceId :: Lens' SpotInstanceRequest (Maybe Text)
siInstanceId = lens _siInstanceId (\s a -> s { _siInstanceId = a })

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
siLaunchGroup :: Lens' SpotInstanceRequest (Maybe Text)
siLaunchGroup = lens _siLaunchGroup (\s a -> s { _siLaunchGroup = a })

-- | Additional information for launching instances.
siLaunchSpecification :: Lens' SpotInstanceRequest (Maybe LaunchSpecification)
siLaunchSpecification =
    lens _siLaunchSpecification (\s a -> s { _siLaunchSpecification = a })

-- | The Availability Zone in which the bid is launched.
siLaunchedAvailabilityZone :: Lens' SpotInstanceRequest (Maybe Text)
siLaunchedAvailabilityZone =
    lens _siLaunchedAvailabilityZone
        (\s a -> s { _siLaunchedAvailabilityZone = a })

-- | The product description associated with the Spot Instance.
siProductDescription :: Lens' SpotInstanceRequest (Maybe RIProductDescription)
siProductDescription =
    lens _siProductDescription (\s a -> s { _siProductDescription = a })

-- | The ID of the Spot Instance request.
siSpotInstanceRequestId :: Lens' SpotInstanceRequest (Maybe Text)
siSpotInstanceRequestId =
    lens _siSpotInstanceRequestId (\s a -> s { _siSpotInstanceRequestId = a })

-- | The maximum hourly price (bid) for any Spot Instance launched to fulfill the
-- request.
siSpotPrice :: Lens' SpotInstanceRequest (Maybe Text)
siSpotPrice = lens _siSpotPrice (\s a -> s { _siSpotPrice = a })

-- | The state of the Spot Instance request. Spot bid status information can help
-- you track your Spot Instance requests. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot BidStatus> in the /Amazon Elastic Compute Cloud User Guide/.
siState :: Lens' SpotInstanceRequest (Maybe SpotInstanceState)
siState = lens _siState (\s a -> s { _siState = a })

-- | The status code and status message describing the Spot Instance request.
siStatus :: Lens' SpotInstanceRequest (Maybe SpotInstanceStatus)
siStatus = lens _siStatus (\s a -> s { _siStatus = a })

-- | Any tags assigned to the resource.
siTags :: Lens' SpotInstanceRequest [Tag]
siTags = lens _siTags (\s a -> s { _siTags = a }) . _List

-- | The Spot Instance request type.
siType :: Lens' SpotInstanceRequest (Maybe SpotInstanceType)
siType = lens _siType (\s a -> s { _siType = a })

-- | The start date of the request, in UTC format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/
-- Z). If this is a one-time request, the request becomes active at this date
-- and time and remains active until all instances launch, the request expires,
-- or the request is canceled. If the request is persistent, the request becomes
-- active at this date and time and remains active until it expires or is
-- canceled.
siValidFrom :: Lens' SpotInstanceRequest (Maybe UTCTime)
siValidFrom = lens _siValidFrom (\s a -> s { _siValidFrom = a }) . mapping _Time

-- | The end date of the request, in UTC format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). If this is a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date is reached.
siValidUntil :: Lens' SpotInstanceRequest (Maybe UTCTime)
siValidUntil = lens _siValidUntil (\s a -> s { _siValidUntil = a }) . mapping _Time

instance FromXML SpotInstanceRequest where
    parseXML x = SpotInstanceRequest
        <$> x .@? "availabilityZoneGroup"
        <*> x .@? "createTime"
        <*> x .@? "fault"
        <*> x .@? "instanceId"
        <*> x .@? "launchGroup"
        <*> x .@? "launchSpecification"
        <*> x .@? "launchedAvailabilityZone"
        <*> x .@? "productDescription"
        <*> x .@? "spotInstanceRequestId"
        <*> x .@? "spotPrice"
        <*> x .@? "state"
        <*> x .@? "status"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "type"
        <*> x .@? "validFrom"
        <*> x .@? "validUntil"

instance ToQuery SpotInstanceRequest where
    toQuery SpotInstanceRequest{..} = mconcat
        [ "AvailabilityZoneGroup"    =? _siAvailabilityZoneGroup
        , "CreateTime"               =? _siCreateTime
        , "Fault"                    =? _siFault
        , "InstanceId"               =? _siInstanceId
        , "LaunchGroup"              =? _siLaunchGroup
        , "LaunchSpecification"      =? _siLaunchSpecification
        , "LaunchedAvailabilityZone" =? _siLaunchedAvailabilityZone
        , "ProductDescription"       =? _siProductDescription
        , "SpotInstanceRequestId"    =? _siSpotInstanceRequestId
        , "SpotPrice"                =? _siSpotPrice
        , "State"                    =? _siState
        , "Status"                   =? _siStatus
        , "TagSet"                   `toQueryList` _siTags
        , "Type"                     =? _siType
        , "ValidFrom"                =? _siValidFrom
        , "ValidUntil"               =? _siValidUntil
        ]

data LaunchSpecification = LaunchSpecification
    { _lsAddressingType      :: Maybe Text
    , _lsBlockDeviceMappings :: List "item" BlockDeviceMapping
    , _lsEbsOptimized        :: Maybe Bool
    , _lsIamInstanceProfile  :: Maybe IamInstanceProfileSpecification
    , _lsImageId             :: Maybe Text
    , _lsInstanceType        :: Maybe InstanceType
    , _lsKernelId            :: Maybe Text
    , _lsKeyName             :: Maybe Text
    , _lsMonitoring          :: Maybe RunInstancesMonitoringEnabled
    , _lsNetworkInterfaces   :: List "item" InstanceNetworkInterfaceSpecification
    , _lsPlacement           :: Maybe SpotPlacement
    , _lsRamdiskId           :: Maybe Text
    , _lsSecurityGroups      :: List "item" GroupIdentifier
    , _lsSubnetId            :: Maybe Text
    , _lsUserData            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'LaunchSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsAddressingType' @::@ 'Maybe' 'Text'
--
-- * 'lsBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'lsEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'lsIamInstanceProfile' @::@ 'Maybe' 'IamInstanceProfileSpecification'
--
-- * 'lsImageId' @::@ 'Maybe' 'Text'
--
-- * 'lsInstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'lsKernelId' @::@ 'Maybe' 'Text'
--
-- * 'lsKeyName' @::@ 'Maybe' 'Text'
--
-- * 'lsMonitoring' @::@ 'Maybe' 'RunInstancesMonitoringEnabled'
--
-- * 'lsNetworkInterfaces' @::@ ['InstanceNetworkInterfaceSpecification']
--
-- * 'lsPlacement' @::@ 'Maybe' 'SpotPlacement'
--
-- * 'lsRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'lsSecurityGroups' @::@ ['GroupIdentifier']
--
-- * 'lsSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'lsUserData' @::@ 'Maybe' 'Text'
--
launchSpecification :: LaunchSpecification
launchSpecification = LaunchSpecification
    { _lsImageId             = Nothing
    , _lsKeyName             = Nothing
    , _lsSecurityGroups      = mempty
    , _lsUserData            = Nothing
    , _lsAddressingType      = Nothing
    , _lsInstanceType        = Nothing
    , _lsPlacement           = Nothing
    , _lsKernelId            = Nothing
    , _lsRamdiskId           = Nothing
    , _lsBlockDeviceMappings = mempty
    , _lsSubnetId            = Nothing
    , _lsNetworkInterfaces   = mempty
    , _lsIamInstanceProfile  = Nothing
    , _lsEbsOptimized        = Nothing
    , _lsMonitoring          = Nothing
    }

-- | Deprecated.
lsAddressingType :: Lens' LaunchSpecification (Maybe Text)
lsAddressingType = lens _lsAddressingType (\s a -> s { _lsAddressingType = a })

-- | One or more block device mapping entries.
lsBlockDeviceMappings :: Lens' LaunchSpecification [BlockDeviceMapping]
lsBlockDeviceMappings =
    lens _lsBlockDeviceMappings (\s a -> s { _lsBlockDeviceMappings = a })
        . _List

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when using
-- an EBS Optimized instance.
--
-- Default: 'false'
lsEbsOptimized :: Lens' LaunchSpecification (Maybe Bool)
lsEbsOptimized = lens _lsEbsOptimized (\s a -> s { _lsEbsOptimized = a })

-- | The IAM instance profile.
lsIamInstanceProfile :: Lens' LaunchSpecification (Maybe IamInstanceProfileSpecification)
lsIamInstanceProfile =
    lens _lsIamInstanceProfile (\s a -> s { _lsIamInstanceProfile = a })

-- | The ID of the AMI.
lsImageId :: Lens' LaunchSpecification (Maybe Text)
lsImageId = lens _lsImageId (\s a -> s { _lsImageId = a })

-- | The instance type.
lsInstanceType :: Lens' LaunchSpecification (Maybe InstanceType)
lsInstanceType = lens _lsInstanceType (\s a -> s { _lsInstanceType = a })

-- | The ID of the kernel.
lsKernelId :: Lens' LaunchSpecification (Maybe Text)
lsKernelId = lens _lsKernelId (\s a -> s { _lsKernelId = a })

-- | The name of the key pair.
lsKeyName :: Lens' LaunchSpecification (Maybe Text)
lsKeyName = lens _lsKeyName (\s a -> s { _lsKeyName = a })

lsMonitoring :: Lens' LaunchSpecification (Maybe RunInstancesMonitoringEnabled)
lsMonitoring = lens _lsMonitoring (\s a -> s { _lsMonitoring = a })

-- | One or more network interfaces.
lsNetworkInterfaces :: Lens' LaunchSpecification [InstanceNetworkInterfaceSpecification]
lsNetworkInterfaces =
    lens _lsNetworkInterfaces (\s a -> s { _lsNetworkInterfaces = a })
        . _List

-- | The placement information for the instance.
lsPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
lsPlacement = lens _lsPlacement (\s a -> s { _lsPlacement = a })

-- | The ID of the RAM disk.
lsRamdiskId :: Lens' LaunchSpecification (Maybe Text)
lsRamdiskId = lens _lsRamdiskId (\s a -> s { _lsRamdiskId = a })

-- | One or more security groups. To request an instance in a nondefault VPC, you
-- must specify the ID of the security group. To request an instance in
-- EC2-Classic or a default VPC, you can specify the name or the ID of the
-- security group.
lsSecurityGroups :: Lens' LaunchSpecification [GroupIdentifier]
lsSecurityGroups = lens _lsSecurityGroups (\s a -> s { _lsSecurityGroups = a }) . _List

-- | The ID of the subnet in which to launch the instance.
lsSubnetId :: Lens' LaunchSpecification (Maybe Text)
lsSubnetId = lens _lsSubnetId (\s a -> s { _lsSubnetId = a })

-- | The Base64-encoded MIME user data to make available to the instances.
lsUserData :: Lens' LaunchSpecification (Maybe Text)
lsUserData = lens _lsUserData (\s a -> s { _lsUserData = a })

instance FromXML LaunchSpecification where
    parseXML x = LaunchSpecification
        <$> x .@? "addressingType"
        <*> x .@? "blockDeviceMapping" .!@ mempty
        <*> x .@? "ebsOptimized"
        <*> x .@? "iamInstanceProfile"
        <*> x .@? "imageId"
        <*> x .@? "instanceType"
        <*> x .@? "kernelId"
        <*> x .@? "keyName"
        <*> x .@? "monitoring"
        <*> x .@? "networkInterfaceSet" .!@ mempty
        <*> x .@? "placement"
        <*> x .@? "ramdiskId"
        <*> x .@? "groupSet" .!@ mempty
        <*> x .@? "subnetId"
        <*> x .@? "userData"

instance ToQuery LaunchSpecification where
    toQuery LaunchSpecification{..} = mconcat
        [ "AddressingType"      =? _lsAddressingType
        , "BlockDeviceMapping"  `toQueryList` _lsBlockDeviceMappings
        , "EbsOptimized"        =? _lsEbsOptimized
        , "IamInstanceProfile"  =? _lsIamInstanceProfile
        , "ImageId"             =? _lsImageId
        , "InstanceType"        =? _lsInstanceType
        , "KernelId"            =? _lsKernelId
        , "KeyName"             =? _lsKeyName
        , "Monitoring"          =? _lsMonitoring
        , "NetworkInterfaceSet" `toQueryList` _lsNetworkInterfaces
        , "Placement"           =? _lsPlacement
        , "RamdiskId"           =? _lsRamdiskId
        , "GroupSet"            `toQueryList` _lsSecurityGroups
        , "SubnetId"            =? _lsSubnetId
        , "UserData"            =? _lsUserData
        ]

data VolumeStatusEvent = VolumeStatusEvent
    { _vseDescription :: Maybe Text
    , _vseEventId     :: Maybe Text
    , _vseEventType   :: Maybe Text
    , _vseNotAfter    :: Maybe ISO8601
    , _vseNotBefore   :: Maybe ISO8601
    } deriving (Eq, Ord, Read, Show)

-- | 'VolumeStatusEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vseDescription' @::@ 'Maybe' 'Text'
--
-- * 'vseEventId' @::@ 'Maybe' 'Text'
--
-- * 'vseEventType' @::@ 'Maybe' 'Text'
--
-- * 'vseNotAfter' @::@ 'Maybe' 'UTCTime'
--
-- * 'vseNotBefore' @::@ 'Maybe' 'UTCTime'
--
volumeStatusEvent :: VolumeStatusEvent
volumeStatusEvent = VolumeStatusEvent
    { _vseEventType   = Nothing
    , _vseDescription = Nothing
    , _vseNotBefore   = Nothing
    , _vseNotAfter    = Nothing
    , _vseEventId     = Nothing
    }

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription = lens _vseDescription (\s a -> s { _vseDescription = a })

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId = lens _vseEventId (\s a -> s { _vseEventId = a })

-- | The type of this event.
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType = lens _vseEventType (\s a -> s { _vseEventType = a })

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotAfter = lens _vseNotAfter (\s a -> s { _vseNotAfter = a }) . mapping _Time

-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotBefore = lens _vseNotBefore (\s a -> s { _vseNotBefore = a }) . mapping _Time

instance FromXML VolumeStatusEvent where
    parseXML x = VolumeStatusEvent
        <$> x .@? "description"
        <*> x .@? "eventId"
        <*> x .@? "eventType"
        <*> x .@? "notAfter"
        <*> x .@? "notBefore"

instance ToQuery VolumeStatusEvent where
    toQuery VolumeStatusEvent{..} = mconcat
        [ "Description" =? _vseDescription
        , "EventId"     =? _vseEventId
        , "EventType"   =? _vseEventType
        , "NotAfter"    =? _vseNotAfter
        , "NotBefore"   =? _vseNotBefore
        ]

data Volume = Volume
    { _vAttachments      :: List "item" VolumeAttachment
    , _vAvailabilityZone :: Text
    , _vCreateTime       :: ISO8601
    , _vEncrypted        :: Bool
    , _vIops             :: Maybe Int
    , _vKmsKeyId         :: Maybe Text
    , _vSize             :: Int
    , _vSnapshotId       :: Text
    , _vState            :: VolumeState
    , _vTags             :: List "item" Tag
    , _vVolumeId         :: Text
    , _vVolumeType       :: VolumeType
    } deriving (Eq, Read, Show)

-- | 'Volume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vAttachments' @::@ ['VolumeAttachment']
--
-- * 'vAvailabilityZone' @::@ 'Text'
--
-- * 'vCreateTime' @::@ 'UTCTime'
--
-- * 'vEncrypted' @::@ 'Bool'
--
-- * 'vIops' @::@ 'Maybe' 'Int'
--
-- * 'vKmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'vSize' @::@ 'Int'
--
-- * 'vSnapshotId' @::@ 'Text'
--
-- * 'vState' @::@ 'VolumeState'
--
-- * 'vTags' @::@ ['Tag']
--
-- * 'vVolumeId' @::@ 'Text'
--
-- * 'vVolumeType' @::@ 'VolumeType'
--
volume :: Text -- ^ 'vVolumeId'
       -> Int -- ^ 'vSize'
       -> Text -- ^ 'vSnapshotId'
       -> Text -- ^ 'vAvailabilityZone'
       -> VolumeState -- ^ 'vState'
       -> UTCTime -- ^ 'vCreateTime'
       -> VolumeType -- ^ 'vVolumeType'
       -> Bool -- ^ 'vEncrypted'
       -> Volume
volume p1 p2 p3 p4 p5 p6 p7 p8 = Volume
    { _vVolumeId         = p1
    , _vSize             = p2
    , _vSnapshotId       = p3
    , _vAvailabilityZone = p4
    , _vState            = p5
    , _vCreateTime       = withIso _Time (const id) p6
    , _vVolumeType       = p7
    , _vEncrypted        = p8
    , _vAttachments      = mempty
    , _vTags             = mempty
    , _vIops             = Nothing
    , _vKmsKeyId         = Nothing
    }

-- | Information about the volume attachments.
vAttachments :: Lens' Volume [VolumeAttachment]
vAttachments = lens _vAttachments (\s a -> s { _vAttachments = a }) . _List

-- | The Availability Zone for the volume.
vAvailabilityZone :: Lens' Volume Text
vAvailabilityZone =
    lens _vAvailabilityZone (\s a -> s { _vAvailabilityZone = a })

-- | The time stamp when volume creation was initiated.
vCreateTime :: Lens' Volume UTCTime
vCreateTime = lens _vCreateTime (\s a -> s { _vCreateTime = a }) . _Time

-- | Indicates whether the volume will be encrypted.
vEncrypted :: Lens' Volume Bool
vEncrypted = lens _vEncrypted (\s a -> s { _vEncrypted = a })

-- | The number of I/O operations per second (IOPS) that the volume supports. For
-- Provisioned IOPS (SSD) volumes, this represents the number of IOPS that are
-- provisioned for the volume. For General Purpose (SSD) volumes, this
-- represents the baseline performance of the volume and the rate at which the
-- volume accumulates I/O credits for bursting. For more information on General
-- Purpose (SSD) baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBSVolume Types> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes and 3
-- to 10000 for General Purpose (SSD) volumes.
--
-- Condition: This parameter is required for requests to create 'io1' volumes; it
-- is not used in requests to create 'standard' or 'gp2' volumes.
vIops :: Lens' Volume (Maybe Int)
vIops = lens _vIops (\s a -> s { _vIops = a })

-- | The full ARN of the AWS Key Management Service (KMS) master key that was used
-- to protect the volume encryption key for the volume.
vKmsKeyId :: Lens' Volume (Maybe Text)
vKmsKeyId = lens _vKmsKeyId (\s a -> s { _vKmsKeyId = a })

-- | The size of the volume, in GiBs.
vSize :: Lens' Volume Int
vSize = lens _vSize (\s a -> s { _vSize = a })

-- | The snapshot from which the volume was created, if applicable.
vSnapshotId :: Lens' Volume Text
vSnapshotId = lens _vSnapshotId (\s a -> s { _vSnapshotId = a })

-- | The volume state.
vState :: Lens' Volume VolumeState
vState = lens _vState (\s a -> s { _vState = a })

-- | Any tags assigned to the volume.
vTags :: Lens' Volume [Tag]
vTags = lens _vTags (\s a -> s { _vTags = a }) . _List

-- | The ID of the volume.
vVolumeId :: Lens' Volume Text
vVolumeId = lens _vVolumeId (\s a -> s { _vVolumeId = a })

-- | The volume type. This can be 'gp2' for General Purpose (SSD) volumes, 'io1' for
-- Provisioned IOPS (SSD) volumes, or 'standard' for Magnetic volumes.
vVolumeType :: Lens' Volume VolumeType
vVolumeType = lens _vVolumeType (\s a -> s { _vVolumeType = a })

instance FromXML Volume where
    parseXML x = Volume
        <$> x .@? "attachmentSet" .!@ mempty
        <*> x .@  "availabilityZone"
        <*> x .@  "createTime"
        <*> x .@  "encrypted"
        <*> x .@? "iops"
        <*> x .@? "kmsKeyId"
        <*> x .@  "size"
        <*> x .@  "snapshotId"
        <*> x .@  "status"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "volumeId"
        <*> x .@  "volumeType"

instance ToQuery Volume where
    toQuery Volume{..} = mconcat
        [ "AttachmentSet"    `toQueryList` _vAttachments
        , "AvailabilityZone" =? _vAvailabilityZone
        , "CreateTime"       =? _vCreateTime
        , "Encrypted"        =? _vEncrypted
        , "Iops"             =? _vIops
        , "KmsKeyId"         =? _vKmsKeyId
        , "Size"             =? _vSize
        , "SnapshotId"       =? _vSnapshotId
        , "Status"           =? _vState
        , "TagSet"           `toQueryList` _vTags
        , "VolumeId"         =? _vVolumeId
        , "VolumeType"       =? _vVolumeType
        ]

data Reservation = Reservation
    { _rGroups        :: List "item" GroupIdentifier
    , _rInstances     :: List "item" Instance
    , _rOwnerId       :: Text
    , _rRequesterId   :: Maybe Text
    , _rReservationId :: Text
    } deriving (Eq, Read, Show)

-- | 'Reservation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rGroups' @::@ ['GroupIdentifier']
--
-- * 'rInstances' @::@ ['Instance']
--
-- * 'rOwnerId' @::@ 'Text'
--
-- * 'rRequesterId' @::@ 'Maybe' 'Text'
--
-- * 'rReservationId' @::@ 'Text'
--
reservation :: Text -- ^ 'rReservationId'
            -> Text -- ^ 'rOwnerId'
            -> Reservation
reservation p1 p2 = Reservation
    { _rReservationId = p1
    , _rOwnerId       = p2
    , _rRequesterId   = Nothing
    , _rGroups        = mempty
    , _rInstances     = mempty
    }

-- | One or more security groups.
rGroups :: Lens' Reservation [GroupIdentifier]
rGroups = lens _rGroups (\s a -> s { _rGroups = a }) . _List

-- | One or more instances.
rInstances :: Lens' Reservation [Instance]
rInstances = lens _rInstances (\s a -> s { _rInstances = a }) . _List

-- | The ID of the AWS account that owns the reservation.
rOwnerId :: Lens' Reservation Text
rOwnerId = lens _rOwnerId (\s a -> s { _rOwnerId = a })

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
rRequesterId :: Lens' Reservation (Maybe Text)
rRequesterId = lens _rRequesterId (\s a -> s { _rRequesterId = a })

-- | The ID of the reservation.
rReservationId :: Lens' Reservation Text
rReservationId = lens _rReservationId (\s a -> s { _rReservationId = a })

instance FromXML Reservation where
    parseXML x = Reservation
        <$> x .@? "groupSet" .!@ mempty
        <*> x .@? "instancesSet" .!@ mempty
        <*> x .@  "ownerId"
        <*> x .@? "requesterId"
        <*> x .@  "reservationId"

instance ToQuery Reservation where
    toQuery Reservation{..} = mconcat
        [ "GroupSet"      `toQueryList` _rGroups
        , "InstancesSet"  `toQueryList` _rInstances
        , "OwnerId"       =? _rOwnerId
        , "RequesterId"   =? _rRequesterId
        , "ReservationId" =? _rReservationId
        ]

data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { _iivdiAvailabilityZone :: Text
    , _iivdiBytesConverted   :: Integer
    , _iivdiDescription      :: Maybe Text
    , _iivdiImage            :: DiskImageDescription
    , _iivdiStatus           :: Text
    , _iivdiStatusMessage    :: Maybe Text
    , _iivdiVolume           :: DiskImageVolumeDescription
    } deriving (Eq, Read, Show)

-- | 'ImportInstanceVolumeDetailItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iivdiAvailabilityZone' @::@ 'Text'
--
-- * 'iivdiBytesConverted' @::@ 'Integer'
--
-- * 'iivdiDescription' @::@ 'Maybe' 'Text'
--
-- * 'iivdiImage' @::@ 'DiskImageDescription'
--
-- * 'iivdiStatus' @::@ 'Text'
--
-- * 'iivdiStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'iivdiVolume' @::@ 'DiskImageVolumeDescription'
--
importInstanceVolumeDetailItem :: Integer -- ^ 'iivdiBytesConverted'
                               -> Text -- ^ 'iivdiAvailabilityZone'
                               -> DiskImageDescription -- ^ 'iivdiImage'
                               -> DiskImageVolumeDescription -- ^ 'iivdiVolume'
                               -> Text -- ^ 'iivdiStatus'
                               -> ImportInstanceVolumeDetailItem
importInstanceVolumeDetailItem p1 p2 p3 p4 p5 = ImportInstanceVolumeDetailItem
    { _iivdiBytesConverted   = p1
    , _iivdiAvailabilityZone = p2
    , _iivdiImage            = p3
    , _iivdiVolume           = p4
    , _iivdiStatus           = p5
    , _iivdiStatusMessage    = Nothing
    , _iivdiDescription      = Nothing
    }

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone :: Lens' ImportInstanceVolumeDetailItem Text
iivdiAvailabilityZone =
    lens _iivdiAvailabilityZone (\s a -> s { _iivdiAvailabilityZone = a })

-- | The number of bytes converted so far.
iivdiBytesConverted :: Lens' ImportInstanceVolumeDetailItem Integer
iivdiBytesConverted =
    lens _iivdiBytesConverted (\s a -> s { _iivdiBytesConverted = a })

-- | A description of the task.
iivdiDescription :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiDescription = lens _iivdiDescription (\s a -> s { _iivdiDescription = a })

-- | The image.
iivdiImage :: Lens' ImportInstanceVolumeDetailItem DiskImageDescription
iivdiImage = lens _iivdiImage (\s a -> s { _iivdiImage = a })

-- | The status of the import of this particular disk image.
iivdiStatus :: Lens' ImportInstanceVolumeDetailItem Text
iivdiStatus = lens _iivdiStatus (\s a -> s { _iivdiStatus = a })

-- | The status information or errors related to the disk image.
iivdiStatusMessage :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiStatusMessage =
    lens _iivdiStatusMessage (\s a -> s { _iivdiStatusMessage = a })

-- | The volume.
iivdiVolume :: Lens' ImportInstanceVolumeDetailItem DiskImageVolumeDescription
iivdiVolume = lens _iivdiVolume (\s a -> s { _iivdiVolume = a })

instance FromXML ImportInstanceVolumeDetailItem where
    parseXML x = ImportInstanceVolumeDetailItem
        <$> x .@  "availabilityZone"
        <*> x .@  "bytesConverted"
        <*> x .@? "description"
        <*> x .@  "image"
        <*> x .@  "status"
        <*> x .@? "statusMessage"
        <*> x .@  "volume"

instance ToQuery ImportInstanceVolumeDetailItem where
    toQuery ImportInstanceVolumeDetailItem{..} = mconcat
        [ "AvailabilityZone" =? _iivdiAvailabilityZone
        , "BytesConverted"   =? _iivdiBytesConverted
        , "Description"      =? _iivdiDescription
        , "Image"            =? _iivdiImage
        , "Status"           =? _iivdiStatus
        , "StatusMessage"    =? _iivdiStatusMessage
        , "Volume"           =? _iivdiVolume
        ]

data SummaryStatus
    = SSImpaired         -- ^ impaired
    | SSInsufficientData -- ^ insufficient-data
    | SSNotApplicable    -- ^ not-applicable
    | SSOk               -- ^ ok
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SummaryStatus

instance FromText SummaryStatus where
    parser = takeLowerText >>= \case
        "impaired"          -> pure SSImpaired
        "insufficient-data" -> pure SSInsufficientData
        "not-applicable"    -> pure SSNotApplicable
        "ok"                -> pure SSOk
        e                   -> fail $
            "Failure parsing SummaryStatus from " ++ show e

instance ToText SummaryStatus where
    toText = \case
        SSImpaired         -> "impaired"
        SSInsufficientData -> "insufficient-data"
        SSNotApplicable    -> "not-applicable"
        SSOk               -> "ok"

instance ToByteString SummaryStatus
instance ToHeader     SummaryStatus
instance ToQuery      SummaryStatus

instance FromXML SummaryStatus where
    parseXML = parseXMLText "SummaryStatus"

data ReservedInstancesModification = ReservedInstancesModification
    { _rimClientToken                     :: Maybe Text
    , _rimCreateDate                      :: Maybe ISO8601
    , _rimEffectiveDate                   :: Maybe ISO8601
    , _rimModificationResults             :: List "item" ReservedInstancesModificationResult
    , _rimReservedInstancesIds            :: List "item" ReservedInstancesId
    , _rimReservedInstancesModificationId :: Maybe Text
    , _rimStatus                          :: Maybe Text
    , _rimStatusMessage                   :: Maybe Text
    , _rimUpdateDate                      :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'ReservedInstancesModification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimClientToken' @::@ 'Maybe' 'Text'
--
-- * 'rimCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rimEffectiveDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rimModificationResults' @::@ ['ReservedInstancesModificationResult']
--
-- * 'rimReservedInstancesIds' @::@ ['ReservedInstancesId']
--
-- * 'rimReservedInstancesModificationId' @::@ 'Maybe' 'Text'
--
-- * 'rimStatus' @::@ 'Maybe' 'Text'
--
-- * 'rimStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'rimUpdateDate' @::@ 'Maybe' 'UTCTime'
--
reservedInstancesModification :: ReservedInstancesModification
reservedInstancesModification = ReservedInstancesModification
    { _rimReservedInstancesModificationId = Nothing
    , _rimReservedInstancesIds            = mempty
    , _rimModificationResults             = mempty
    , _rimCreateDate                      = Nothing
    , _rimUpdateDate                      = Nothing
    , _rimEffectiveDate                   = Nothing
    , _rimStatus                          = Nothing
    , _rimStatusMessage                   = Nothing
    , _rimClientToken                     = Nothing
    }

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
rimClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rimClientToken = lens _rimClientToken (\s a -> s { _rimClientToken = a })

-- | The time when the modification request was created.
rimCreateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimCreateDate = lens _rimCreateDate (\s a -> s { _rimCreateDate = a }) . mapping _Time

-- | The time for the modification to become effective.
rimEffectiveDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimEffectiveDate = lens _rimEffectiveDate (\s a -> s { _rimEffectiveDate = a }) . mapping _Time

-- | Contains target configurations along with their corresponding new Reserved
-- Instance IDs.
rimModificationResults :: Lens' ReservedInstancesModification [ReservedInstancesModificationResult]
rimModificationResults =
    lens _rimModificationResults (\s a -> s { _rimModificationResults = a })
        . _List

-- | The IDs of one or more Reserved Instances.
rimReservedInstancesIds :: Lens' ReservedInstancesModification [ReservedInstancesId]
rimReservedInstancesIds =
    lens _rimReservedInstancesIds (\s a -> s { _rimReservedInstancesIds = a })
        . _List

-- | A unique ID for the Reserved Instance modification.
rimReservedInstancesModificationId :: Lens' ReservedInstancesModification (Maybe Text)
rimReservedInstancesModificationId =
    lens _rimReservedInstancesModificationId
        (\s a -> s { _rimReservedInstancesModificationId = a })

-- | The status of the Reserved Instances modification request.
rimStatus :: Lens' ReservedInstancesModification (Maybe Text)
rimStatus = lens _rimStatus (\s a -> s { _rimStatus = a })

-- | The reason for the status.
rimStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rimStatusMessage = lens _rimStatusMessage (\s a -> s { _rimStatusMessage = a })

-- | The time when the modification request was last updated.
rimUpdateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimUpdateDate = lens _rimUpdateDate (\s a -> s { _rimUpdateDate = a }) . mapping _Time

instance FromXML ReservedInstancesModification where
    parseXML x = ReservedInstancesModification
        <$> x .@? "clientToken"
        <*> x .@? "createDate"
        <*> x .@? "effectiveDate"
        <*> x .@? "modificationResultSet" .!@ mempty
        <*> x .@? "reservedInstancesSet" .!@ mempty
        <*> x .@? "reservedInstancesModificationId"
        <*> x .@? "status"
        <*> x .@? "statusMessage"
        <*> x .@? "updateDate"

instance ToQuery ReservedInstancesModification where
    toQuery ReservedInstancesModification{..} = mconcat
        [ "ClientToken"                     =? _rimClientToken
        , "CreateDate"                      =? _rimCreateDate
        , "EffectiveDate"                   =? _rimEffectiveDate
        , "ModificationResultSet"           `toQueryList` _rimModificationResults
        , "ReservedInstancesSet"            `toQueryList` _rimReservedInstancesIds
        , "ReservedInstancesModificationId" =? _rimReservedInstancesModificationId
        , "Status"                          =? _rimStatus
        , "StatusMessage"                   =? _rimStatusMessage
        , "UpdateDate"                      =? _rimUpdateDate
        ]

data RuleAction
    = Allow -- ^ allow
    | Deny  -- ^ deny
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable RuleAction

instance FromText RuleAction where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "deny"  -> pure Deny
        e       -> fail $
            "Failure parsing RuleAction from " ++ show e

instance ToText RuleAction where
    toText = \case
        Allow -> "allow"
        Deny  -> "deny"

instance ToByteString RuleAction
instance ToHeader     RuleAction
instance ToQuery      RuleAction

instance FromXML RuleAction where
    parseXML = parseXMLText "RuleAction"

data BatchState
    = Active               -- ^ active
    | Cancelled            -- ^ cancelled
    | CancelledRunning     -- ^ cancelled_running
    | CancelledTerminating -- ^ cancelled_terminating
    | Failed               -- ^ failed
    | Submitted            -- ^ submitted
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable BatchState

instance FromText BatchState where
    parser = takeLowerText >>= \case
        "active"                -> pure Active
        "cancelled"             -> pure Cancelled
        "cancelled_running"     -> pure CancelledRunning
        "cancelled_terminating" -> pure CancelledTerminating
        "failed"                -> pure Failed
        "submitted"             -> pure Submitted
        e                       -> fail $
            "Failure parsing BatchState from " ++ show e

instance ToText BatchState where
    toText = \case
        Active               -> "active"
        Cancelled            -> "cancelled"
        CancelledRunning     -> "cancelled_running"
        CancelledTerminating -> "cancelled_terminating"
        Failed               -> "failed"
        Submitted            -> "submitted"

instance ToByteString BatchState
instance ToHeader     BatchState
instance ToQuery      BatchState

instance FromXML BatchState where
    parseXML = parseXMLText "BatchState"

data NetworkInterface = NetworkInterface
    { _niAssociation        :: Maybe NetworkInterfaceAssociation
    , _niAttachment         :: Maybe NetworkInterfaceAttachment
    , _niAvailabilityZone   :: Maybe Text
    , _niDescription        :: Maybe Text
    , _niGroups             :: List "item" GroupIdentifier
    , _niMacAddress         :: Maybe Text
    , _niNetworkInterfaceId :: Maybe Text
    , _niOwnerId            :: Maybe Text
    , _niPrivateDnsName     :: Maybe Text
    , _niPrivateIpAddress   :: Maybe Text
    , _niPrivateIpAddresses :: List "item" NetworkInterfacePrivateIpAddress
    , _niRequesterId        :: Maybe Text
    , _niRequesterManaged   :: Maybe Bool
    , _niSourceDestCheck    :: Maybe Bool
    , _niStatus             :: Maybe NetworkInterfaceStatus
    , _niSubnetId           :: Maybe Text
    , _niTagSet             :: List "item" Tag
    , _niVpcId              :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'NetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niAssociation' @::@ 'Maybe' 'NetworkInterfaceAssociation'
--
-- * 'niAttachment' @::@ 'Maybe' 'NetworkInterfaceAttachment'
--
-- * 'niAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'niDescription' @::@ 'Maybe' 'Text'
--
-- * 'niGroups' @::@ ['GroupIdentifier']
--
-- * 'niMacAddress' @::@ 'Maybe' 'Text'
--
-- * 'niNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'niOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'niPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'niPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'niPrivateIpAddresses' @::@ ['NetworkInterfacePrivateIpAddress']
--
-- * 'niRequesterId' @::@ 'Maybe' 'Text'
--
-- * 'niRequesterManaged' @::@ 'Maybe' 'Bool'
--
-- * 'niSourceDestCheck' @::@ 'Maybe' 'Bool'
--
-- * 'niStatus' @::@ 'Maybe' 'NetworkInterfaceStatus'
--
-- * 'niSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'niTagSet' @::@ ['Tag']
--
-- * 'niVpcId' @::@ 'Maybe' 'Text'
--
networkInterface :: NetworkInterface
networkInterface = NetworkInterface
    { _niNetworkInterfaceId = Nothing
    , _niSubnetId           = Nothing
    , _niVpcId              = Nothing
    , _niAvailabilityZone   = Nothing
    , _niDescription        = Nothing
    , _niOwnerId            = Nothing
    , _niRequesterId        = Nothing
    , _niRequesterManaged   = Nothing
    , _niStatus             = Nothing
    , _niMacAddress         = Nothing
    , _niPrivateIpAddress   = Nothing
    , _niPrivateDnsName     = Nothing
    , _niSourceDestCheck    = Nothing
    , _niGroups             = mempty
    , _niAttachment         = Nothing
    , _niAssociation        = Nothing
    , _niTagSet             = mempty
    , _niPrivateIpAddresses = mempty
    }

-- | The association information for an Elastic IP associated with the network
-- interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation = lens _niAssociation (\s a -> s { _niAssociation = a })

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment = lens _niAttachment (\s a -> s { _niAttachment = a })

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone =
    lens _niAvailabilityZone (\s a -> s { _niAvailabilityZone = a })

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription = lens _niDescription (\s a -> s { _niDescription = a })

-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface [GroupIdentifier]
niGroups = lens _niGroups (\s a -> s { _niGroups = a }) . _List

-- | The MAC address.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress = lens _niMacAddress (\s a -> s { _niMacAddress = a })

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId =
    lens _niNetworkInterfaceId (\s a -> s { _niNetworkInterfaceId = a })

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId = lens _niOwnerId (\s a -> s { _niOwnerId = a })

-- | The private DNS name.
niPrivateDnsName :: Lens' NetworkInterface (Maybe Text)
niPrivateDnsName = lens _niPrivateDnsName (\s a -> s { _niPrivateDnsName = a })

-- | The IP address of the network interface within the subnet.
niPrivateIpAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIpAddress =
    lens _niPrivateIpAddress (\s a -> s { _niPrivateIpAddress = a })

-- | The private IP addresses associated with the network interface.
niPrivateIpAddresses :: Lens' NetworkInterface [NetworkInterfacePrivateIpAddress]
niPrivateIpAddresses =
    lens _niPrivateIpAddresses (\s a -> s { _niPrivateIpAddresses = a })
        . _List

-- | The ID of the entity that launched the instance on your behalf (for example,
-- AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId = lens _niRequesterId (\s a -> s { _niRequesterId = a })

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged =
    lens _niRequesterManaged (\s a -> s { _niRequesterManaged = a })

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck =
    lens _niSourceDestCheck (\s a -> s { _niSourceDestCheck = a })

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe NetworkInterfaceStatus)
niStatus = lens _niStatus (\s a -> s { _niStatus = a })

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s { _niSubnetId = a })

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface [Tag]
niTagSet = lens _niTagSet (\s a -> s { _niTagSet = a }) . _List

-- | The ID of the VPC.
niVpcId :: Lens' NetworkInterface (Maybe Text)
niVpcId = lens _niVpcId (\s a -> s { _niVpcId = a })

instance FromXML NetworkInterface where
    parseXML x = NetworkInterface
        <$> x .@? "association"
        <*> x .@? "attachment"
        <*> x .@? "availabilityZone"
        <*> x .@? "description"
        <*> x .@? "groupSet" .!@ mempty
        <*> x .@? "macAddress"
        <*> x .@? "networkInterfaceId"
        <*> x .@? "ownerId"
        <*> x .@? "privateDnsName"
        <*> x .@? "privateIpAddress"
        <*> x .@? "privateIpAddressesSet" .!@ mempty
        <*> x .@? "requesterId"
        <*> x .@? "requesterManaged"
        <*> x .@? "sourceDestCheck"
        <*> x .@? "status"
        <*> x .@? "subnetId"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "vpcId"

instance ToQuery NetworkInterface where
    toQuery NetworkInterface{..} = mconcat
        [ "Association"           =? _niAssociation
        , "Attachment"            =? _niAttachment
        , "AvailabilityZone"      =? _niAvailabilityZone
        , "Description"           =? _niDescription
        , "GroupSet"              `toQueryList` _niGroups
        , "MacAddress"            =? _niMacAddress
        , "NetworkInterfaceId"    =? _niNetworkInterfaceId
        , "OwnerId"               =? _niOwnerId
        , "PrivateDnsName"        =? _niPrivateDnsName
        , "PrivateIpAddress"      =? _niPrivateIpAddress
        , "PrivateIpAddressesSet" `toQueryList` _niPrivateIpAddresses
        , "RequesterId"           =? _niRequesterId
        , "RequesterManaged"      =? _niRequesterManaged
        , "SourceDestCheck"       =? _niSourceDestCheck
        , "Status"                =? _niStatus
        , "SubnetId"              =? _niSubnetId
        , "TagSet"                `toQueryList` _niTagSet
        , "VpcId"                 =? _niVpcId
        ]

data TelemetryStatus
    = Down -- ^ DOWN
    | Up   -- ^ UP
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable TelemetryStatus

instance FromText TelemetryStatus where
    parser = takeLowerText >>= \case
        "down" -> pure Down
        "up"   -> pure Up
        e      -> fail $
            "Failure parsing TelemetryStatus from " ++ show e

instance ToText TelemetryStatus where
    toText = \case
        Down -> "DOWN"
        Up   -> "UP"

instance ToByteString TelemetryStatus
instance ToHeader     TelemetryStatus
instance ToQuery      TelemetryStatus

instance FromXML TelemetryStatus where
    parseXML = parseXMLText "TelemetryStatus"

data Subnet = Subnet
    { _s1AvailabilityZone        :: Text
    , _s1AvailableIpAddressCount :: Int
    , _s1CidrBlock               :: Text
    , _s1DefaultForAz            :: Bool
    , _s1MapPublicIpOnLaunch     :: Bool
    , _s1State                   :: SubnetState
    , _s1SubnetId                :: Text
    , _s1Tags                    :: List "item" Tag
    , _s1VpcId                   :: Text
    } deriving (Eq, Read, Show)

-- | 'Subnet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 's1AvailabilityZone' @::@ 'Text'
--
-- * 's1AvailableIpAddressCount' @::@ 'Int'
--
-- * 's1CidrBlock' @::@ 'Text'
--
-- * 's1DefaultForAz' @::@ 'Bool'
--
-- * 's1MapPublicIpOnLaunch' @::@ 'Bool'
--
-- * 's1State' @::@ 'SubnetState'
--
-- * 's1SubnetId' @::@ 'Text'
--
-- * 's1Tags' @::@ ['Tag']
--
-- * 's1VpcId' @::@ 'Text'
--
subnet :: Text -- ^ 's1SubnetId'
       -> SubnetState -- ^ 's1State'
       -> Text -- ^ 's1VpcId'
       -> Text -- ^ 's1CidrBlock'
       -> Int -- ^ 's1AvailableIpAddressCount'
       -> Text -- ^ 's1AvailabilityZone'
       -> Bool -- ^ 's1DefaultForAz'
       -> Bool -- ^ 's1MapPublicIpOnLaunch'
       -> Subnet
subnet p1 p2 p3 p4 p5 p6 p7 p8 = Subnet
    { _s1SubnetId                = p1
    , _s1State                   = p2
    , _s1VpcId                   = p3
    , _s1CidrBlock               = p4
    , _s1AvailableIpAddressCount = p5
    , _s1AvailabilityZone        = p6
    , _s1DefaultForAz            = p7
    , _s1MapPublicIpOnLaunch     = p8
    , _s1Tags                    = mempty
    }

-- | The Availability Zone of the subnet.
s1AvailabilityZone :: Lens' Subnet Text
s1AvailabilityZone =
    lens _s1AvailabilityZone (\s a -> s { _s1AvailabilityZone = a })

-- | The number of unused IP addresses in the subnet. Note that the IP addresses
-- for any stopped instances are considered unavailable.
s1AvailableIpAddressCount :: Lens' Subnet Int
s1AvailableIpAddressCount =
    lens _s1AvailableIpAddressCount
        (\s a -> s { _s1AvailableIpAddressCount = a })

-- | The CIDR block assigned to the subnet.
s1CidrBlock :: Lens' Subnet Text
s1CidrBlock = lens _s1CidrBlock (\s a -> s { _s1CidrBlock = a })

-- | Indicates whether this is the default subnet for the Availability Zone.
s1DefaultForAz :: Lens' Subnet Bool
s1DefaultForAz = lens _s1DefaultForAz (\s a -> s { _s1DefaultForAz = a })

-- | Indicates whether instances launched in this subnet receive a public IP
-- address.
s1MapPublicIpOnLaunch :: Lens' Subnet Bool
s1MapPublicIpOnLaunch =
    lens _s1MapPublicIpOnLaunch (\s a -> s { _s1MapPublicIpOnLaunch = a })

-- | The current state of the subnet.
s1State :: Lens' Subnet SubnetState
s1State = lens _s1State (\s a -> s { _s1State = a })

-- | The ID of the subnet.
s1SubnetId :: Lens' Subnet Text
s1SubnetId = lens _s1SubnetId (\s a -> s { _s1SubnetId = a })

-- | Any tags assigned to the subnet.
s1Tags :: Lens' Subnet [Tag]
s1Tags = lens _s1Tags (\s a -> s { _s1Tags = a }) . _List

-- | The ID of the VPC the subnet is in.
s1VpcId :: Lens' Subnet Text
s1VpcId = lens _s1VpcId (\s a -> s { _s1VpcId = a })

instance FromXML Subnet where
    parseXML x = Subnet
        <$> x .@  "availabilityZone"
        <*> x .@  "availableIpAddressCount"
        <*> x .@  "cidrBlock"
        <*> x .@  "defaultForAz"
        <*> x .@  "mapPublicIpOnLaunch"
        <*> x .@  "state"
        <*> x .@  "subnetId"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "vpcId"

instance ToQuery Subnet where
    toQuery Subnet{..} = mconcat
        [ "AvailabilityZone"        =? _s1AvailabilityZone
        , "AvailableIpAddressCount" =? _s1AvailableIpAddressCount
        , "CidrBlock"               =? _s1CidrBlock
        , "DefaultForAz"            =? _s1DefaultForAz
        , "MapPublicIpOnLaunch"     =? _s1MapPublicIpOnLaunch
        , "State"                   =? _s1State
        , "SubnetId"                =? _s1SubnetId
        , "TagSet"                  `toQueryList` _s1Tags
        , "VpcId"                   =? _s1VpcId
        ]

data KeyPairInfo = KeyPairInfo
    { _kpiKeyFingerprint :: Maybe Text
    , _kpiKeyName        :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'KeyPairInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kpiKeyFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'kpiKeyName' @::@ 'Maybe' 'Text'
--
keyPairInfo :: KeyPairInfo
keyPairInfo = KeyPairInfo
    { _kpiKeyName        = Nothing
    , _kpiKeyFingerprint = Nothing
    }

-- | If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of
-- the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the
-- public key, this is the MD5 public key fingerprint as specified in section 4
-- of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint =
    lens _kpiKeyFingerprint (\s a -> s { _kpiKeyFingerprint = a })

-- | The name of the key pair.
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName = lens _kpiKeyName (\s a -> s { _kpiKeyName = a })

instance FromXML KeyPairInfo where
    parseXML x = KeyPairInfo
        <$> x .@? "keyFingerprint"
        <*> x .@? "keyName"

instance ToQuery KeyPairInfo where
    toQuery KeyPairInfo{..} = mconcat
        [ "KeyFingerprint" =? _kpiKeyFingerprint
        , "KeyName"        =? _kpiKeyName
        ]

data LaunchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd    :: List "item" LaunchPermission
    , _lpmRemove :: List "item" LaunchPermission
    } deriving (Eq, Read, Show)

-- | 'LaunchPermissionModifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpmAdd' @::@ ['LaunchPermission']
--
-- * 'lpmRemove' @::@ ['LaunchPermission']
--
launchPermissionModifications :: LaunchPermissionModifications
launchPermissionModifications = LaunchPermissionModifications
    { _lpmAdd    = mempty
    , _lpmRemove = mempty
    }

-- | The AWS account ID to add to the list of launch permissions for the AMI.
lpmAdd :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmAdd = lens _lpmAdd (\s a -> s { _lpmAdd = a }) . _List

-- | The AWS account ID to remove from the list of launch permissions for the AMI.
lpmRemove :: Lens' LaunchPermissionModifications [LaunchPermission]
lpmRemove = lens _lpmRemove (\s a -> s { _lpmRemove = a }) . _List

instance FromXML LaunchPermissionModifications where
    parseXML x = LaunchPermissionModifications
        <$> x .@? "Add" .!@ mempty
        <*> x .@? "Remove" .!@ mempty

instance ToQuery LaunchPermissionModifications where
    toQuery LaunchPermissionModifications{..} = mconcat
        [ "Add"    `toQueryList` _lpmAdd
        , "Remove" `toQueryList` _lpmRemove
        ]

data SnapshotState
    = SSCompleted -- ^ completed
    | SSError     -- ^ error
    | SSPending   -- ^ pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SnapshotState

instance FromText SnapshotState where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "error"     -> pure SSError
        "pending"   -> pure SSPending
        e           -> fail $
            "Failure parsing SnapshotState from " ++ show e

instance ToText SnapshotState where
    toText = \case
        SSCompleted -> "completed"
        SSError     -> "error"
        SSPending   -> "pending"

instance ToByteString SnapshotState
instance ToHeader     SnapshotState
instance ToQuery      SnapshotState

instance FromXML SnapshotState where
    parseXML = parseXMLText "SnapshotState"

data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _iniaIpOwnerId     :: Maybe Text
    , _iniaPublicDnsName :: Maybe Text
    , _iniaPublicIp      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'InstanceNetworkInterfaceAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniaIpOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'iniaPublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'iniaPublicIp' @::@ 'Maybe' 'Text'
--
instanceNetworkInterfaceAssociation :: InstanceNetworkInterfaceAssociation
instanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { _iniaPublicIp      = Nothing
    , _iniaPublicDnsName = Nothing
    , _iniaIpOwnerId     = Nothing
    }

-- | The ID of the owner of the Elastic IP address.
iniaIpOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaIpOwnerId = lens _iniaIpOwnerId (\s a -> s { _iniaIpOwnerId = a })

-- | The public DNS name.
iniaPublicDnsName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicDnsName =
    lens _iniaPublicDnsName (\s a -> s { _iniaPublicDnsName = a })

-- | The public IP address or Elastic IP address bound to the network interface.
iniaPublicIp :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicIp = lens _iniaPublicIp (\s a -> s { _iniaPublicIp = a })

instance FromXML InstanceNetworkInterfaceAssociation where
    parseXML x = InstanceNetworkInterfaceAssociation
        <$> x .@? "ipOwnerId"
        <*> x .@? "publicDnsName"
        <*> x .@? "publicIp"

instance ToQuery InstanceNetworkInterfaceAssociation where
    toQuery InstanceNetworkInterfaceAssociation{..} = mconcat
        [ "IpOwnerId"     =? _iniaIpOwnerId
        , "PublicDnsName" =? _iniaPublicDnsName
        , "PublicIp"      =? _iniaPublicIp
        ]

data DiskImageDetail = DiskImageDetail
    { _didBytes             :: Integer
    , _didFormat            :: DiskImageFormat
    , _didImportManifestUrl :: Text
    } deriving (Eq, Read, Show)

-- | 'DiskImageDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'didBytes' @::@ 'Integer'
--
-- * 'didFormat' @::@ 'DiskImageFormat'
--
-- * 'didImportManifestUrl' @::@ 'Text'
--
diskImageDetail :: DiskImageFormat -- ^ 'didFormat'
                -> Integer -- ^ 'didBytes'
                -> Text -- ^ 'didImportManifestUrl'
                -> DiskImageDetail
diskImageDetail p1 p2 p3 = DiskImageDetail
    { _didFormat            = p1
    , _didBytes             = p2
    , _didImportManifestUrl = p3
    }

-- | The size of the disk image, in GiB.
didBytes :: Lens' DiskImageDetail Integer
didBytes = lens _didBytes (\s a -> s { _didBytes = a })

-- | The disk image format.
didFormat :: Lens' DiskImageDetail DiskImageFormat
didFormat = lens _didFormat (\s a -> s { _didFormat = a })

-- | A presigned URL for the import manifest stored in Amazon S3 and presented
-- here as an Amazon S3 presigned URL. For information about creating a
-- presigned URL for an Amazon S3 object, read the "Query String Request
-- Authentication Alternative" section of the <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic
-- in the /Amazon Simple Storage Service Developer Guide/.
didImportManifestUrl :: Lens' DiskImageDetail Text
didImportManifestUrl =
    lens _didImportManifestUrl (\s a -> s { _didImportManifestUrl = a })

instance FromXML DiskImageDetail where
    parseXML x = DiskImageDetail
        <$> x .@  "bytes"
        <*> x .@  "format"
        <*> x .@  "importManifestUrl"

instance ToQuery DiskImageDetail where
    toQuery DiskImageDetail{..} = mconcat
        [ "Bytes"             =? _didBytes
        , "Format"            =? _didFormat
        , "ImportManifestUrl" =? _didImportManifestUrl
        ]

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaAssociation      :: Maybe InstanceNetworkInterfaceAssociation
    , _ipiaPrimary          :: Maybe Bool
    , _ipiaPrivateDnsName   :: Maybe Text
    , _ipiaPrivateIpAddress :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'InstancePrivateIpAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipiaAssociation' @::@ 'Maybe' 'InstanceNetworkInterfaceAssociation'
--
-- * 'ipiaPrimary' @::@ 'Maybe' 'Bool'
--
-- * 'ipiaPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'ipiaPrivateIpAddress' @::@ 'Maybe' 'Text'
--
instancePrivateIpAddress :: InstancePrivateIpAddress
instancePrivateIpAddress = InstancePrivateIpAddress
    { _ipiaPrivateIpAddress = Nothing
    , _ipiaPrivateDnsName   = Nothing
    , _ipiaPrimary          = Nothing
    , _ipiaAssociation      = Nothing
    }

-- | The association information for an Elastic IP address for the network
-- interface.
ipiaAssociation :: Lens' InstancePrivateIpAddress (Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation = lens _ipiaAssociation (\s a -> s { _ipiaAssociation = a })

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
ipiaPrimary :: Lens' InstancePrivateIpAddress (Maybe Bool)
ipiaPrimary = lens _ipiaPrimary (\s a -> s { _ipiaPrimary = a })

-- | The private DNS name.
ipiaPrivateDnsName :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateDnsName =
    lens _ipiaPrivateDnsName (\s a -> s { _ipiaPrivateDnsName = a })

-- | The private IP address of the network interface.
ipiaPrivateIpAddress :: Lens' InstancePrivateIpAddress (Maybe Text)
ipiaPrivateIpAddress =
    lens _ipiaPrivateIpAddress (\s a -> s { _ipiaPrivateIpAddress = a })

instance FromXML InstancePrivateIpAddress where
    parseXML x = InstancePrivateIpAddress
        <$> x .@? "association"
        <*> x .@? "primary"
        <*> x .@? "privateDnsName"
        <*> x .@? "privateIpAddress"

instance ToQuery InstancePrivateIpAddress where
    toQuery InstancePrivateIpAddress{..} = mconcat
        [ "Association"      =? _ipiaAssociation
        , "Primary"          =? _ipiaPrimary
        , "PrivateDnsName"   =? _ipiaPrivateDnsName
        , "PrivateIpAddress" =? _ipiaPrivateIpAddress
        ]

data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csiSpotInstanceRequestId :: Maybe Text
    , _csiState                 :: Maybe CancelSpotInstanceRequestState
    } deriving (Eq, Read, Show)

-- | 'CancelledSpotInstanceRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csiSpotInstanceRequestId' @::@ 'Maybe' 'Text'
--
-- * 'csiState' @::@ 'Maybe' 'CancelSpotInstanceRequestState'
--
cancelledSpotInstanceRequest :: CancelledSpotInstanceRequest
cancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { _csiSpotInstanceRequestId = Nothing
    , _csiState                 = Nothing
    }

-- | The ID of the Spot Instance request.
csiSpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csiSpotInstanceRequestId =
    lens _csiSpotInstanceRequestId
        (\s a -> s { _csiSpotInstanceRequestId = a })

-- | The state of the Spot Instance request.
csiState :: Lens' CancelledSpotInstanceRequest (Maybe CancelSpotInstanceRequestState)
csiState = lens _csiState (\s a -> s { _csiState = a })

instance FromXML CancelledSpotInstanceRequest where
    parseXML x = CancelledSpotInstanceRequest
        <$> x .@? "spotInstanceRequestId"
        <*> x .@? "state"

instance ToQuery CancelledSpotInstanceRequest where
    toQuery CancelledSpotInstanceRequest{..} = mconcat
        [ "SpotInstanceRequestId" =? _csiSpotInstanceRequestId
        , "State"                 =? _csiState
        ]

newtype VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'VpnConnectionOptionsSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcosStaticRoutesOnly' @::@ 'Maybe' 'Bool'
--
vpnConnectionOptionsSpecification :: VpnConnectionOptionsSpecification
vpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { _vcosStaticRoutesOnly = Nothing
    }

-- | Indicates whether the VPN connection uses static routes only. Static routes
-- must be used for devices that don't support BGP.
vcosStaticRoutesOnly :: Lens' VpnConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly =
    lens _vcosStaticRoutesOnly (\s a -> s { _vcosStaticRoutesOnly = a })

instance FromXML VpnConnectionOptionsSpecification where
    parseXML x = VpnConnectionOptionsSpecification
        <$> x .@? "staticRoutesOnly"

instance ToQuery VpnConnectionOptionsSpecification where
    toQuery VpnConnectionOptionsSpecification{..} = mconcat
        [ "StaticRoutesOnly" =? _vcosStaticRoutesOnly
        ]

data Address = Address
    { _aAllocationId            :: Maybe Text
    , _aAssociationId           :: Maybe Text
    , _aDomain                  :: Maybe DomainType
    , _aInstanceId              :: Maybe Text
    , _aNetworkInterfaceId      :: Maybe Text
    , _aNetworkInterfaceOwnerId :: Maybe Text
    , _aPrivateIpAddress        :: Maybe Text
    , _aPublicIp                :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Address' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'aAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'aDomain' @::@ 'Maybe' 'DomainType'
--
-- * 'aInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'aNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'aNetworkInterfaceOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'aPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'aPublicIp' @::@ 'Maybe' 'Text'
--
address :: Address
address = Address
    { _aInstanceId              = Nothing
    , _aPublicIp                = Nothing
    , _aAllocationId            = Nothing
    , _aAssociationId           = Nothing
    , _aDomain                  = Nothing
    , _aNetworkInterfaceId      = Nothing
    , _aNetworkInterfaceOwnerId = Nothing
    , _aPrivateIpAddress        = Nothing
    }

-- | The ID representing the allocation of the address for use with EC2-VPC.
aAllocationId :: Lens' Address (Maybe Text)
aAllocationId = lens _aAllocationId (\s a -> s { _aAllocationId = a })

-- | The ID representing the association of the address with an instance in a VPC.
aAssociationId :: Lens' Address (Maybe Text)
aAssociationId = lens _aAssociationId (\s a -> s { _aAssociationId = a })

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic ('standard') or instances in a VPC ('vpc').
aDomain :: Lens' Address (Maybe DomainType)
aDomain = lens _aDomain (\s a -> s { _aDomain = a })

-- | The ID of the instance that the address is associated with (if any).
aInstanceId :: Lens' Address (Maybe Text)
aInstanceId = lens _aInstanceId (\s a -> s { _aInstanceId = a })

-- | The ID of the network interface.
aNetworkInterfaceId :: Lens' Address (Maybe Text)
aNetworkInterfaceId =
    lens _aNetworkInterfaceId (\s a -> s { _aNetworkInterfaceId = a })

-- | The ID of the AWS account that owns the network interface.
aNetworkInterfaceOwnerId :: Lens' Address (Maybe Text)
aNetworkInterfaceOwnerId =
    lens _aNetworkInterfaceOwnerId
        (\s a -> s { _aNetworkInterfaceOwnerId = a })

-- | The private IP address associated with the Elastic IP address.
aPrivateIpAddress :: Lens' Address (Maybe Text)
aPrivateIpAddress =
    lens _aPrivateIpAddress (\s a -> s { _aPrivateIpAddress = a })

-- | The Elastic IP address.
aPublicIp :: Lens' Address (Maybe Text)
aPublicIp = lens _aPublicIp (\s a -> s { _aPublicIp = a })

instance FromXML Address where
    parseXML x = Address
        <$> x .@? "allocationId"
        <*> x .@? "associationId"
        <*> x .@? "domain"
        <*> x .@? "instanceId"
        <*> x .@? "networkInterfaceId"
        <*> x .@? "networkInterfaceOwnerId"
        <*> x .@? "privateIpAddress"
        <*> x .@? "publicIp"

instance ToQuery Address where
    toQuery Address{..} = mconcat
        [ "AllocationId"            =? _aAllocationId
        , "AssociationId"           =? _aAssociationId
        , "Domain"                  =? _aDomain
        , "InstanceId"              =? _aInstanceId
        , "NetworkInterfaceId"      =? _aNetworkInterfaceId
        , "NetworkInterfaceOwnerId" =? _aNetworkInterfaceOwnerId
        , "PrivateIpAddress"        =? _aPrivateIpAddress
        , "PublicIp"                =? _aPublicIp
        ]

data VolumeAttachmentState
    = VASAttached  -- ^ attached
    | VASAttaching -- ^ attaching
    | VASDetached  -- ^ detached
    | VASDetaching -- ^ detaching
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VolumeAttachmentState

instance FromText VolumeAttachmentState where
    parser = takeLowerText >>= \case
        "attached"  -> pure VASAttached
        "attaching" -> pure VASAttaching
        "detached"  -> pure VASDetached
        "detaching" -> pure VASDetaching
        e           -> fail $
            "Failure parsing VolumeAttachmentState from " ++ show e

instance ToText VolumeAttachmentState where
    toText = \case
        VASAttached  -> "attached"
        VASAttaching -> "attaching"
        VASDetached  -> "detached"
        VASDetaching -> "detaching"

instance ToByteString VolumeAttachmentState
instance ToHeader     VolumeAttachmentState
instance ToQuery      VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    parseXML = parseXMLText "VolumeAttachmentState"

data MovingAddressStatus = MovingAddressStatus
    { _masMoveStatus :: Maybe MoveStatus
    , _masPublicIp   :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'MovingAddressStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'masMoveStatus' @::@ 'Maybe' 'MoveStatus'
--
-- * 'masPublicIp' @::@ 'Maybe' 'Text'
--
movingAddressStatus :: MovingAddressStatus
movingAddressStatus = MovingAddressStatus
    { _masPublicIp   = Nothing
    , _masMoveStatus = Nothing
    }

-- | The status of the Elastic IP address that's being moved to the EC2-VPC
-- platform, or restored to the EC2-Classic platform.
masMoveStatus :: Lens' MovingAddressStatus (Maybe MoveStatus)
masMoveStatus = lens _masMoveStatus (\s a -> s { _masMoveStatus = a })

-- | The Elastic IP address.
masPublicIp :: Lens' MovingAddressStatus (Maybe Text)
masPublicIp = lens _masPublicIp (\s a -> s { _masPublicIp = a })

instance FromXML MovingAddressStatus where
    parseXML x = MovingAddressStatus
        <$> x .@? "moveStatus"
        <*> x .@? "publicIp"

instance ToQuery MovingAddressStatus where
    toQuery MovingAddressStatus{..} = mconcat
        [ "MoveStatus" =? _masMoveStatus
        , "PublicIp"   =? _masPublicIp
        ]

data LaunchPermission = LaunchPermission
    { _lpGroup  :: Maybe PermissionGroup
    , _lpUserId :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'LaunchPermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpGroup' @::@ 'Maybe' 'PermissionGroup'
--
-- * 'lpUserId' @::@ 'Maybe' 'Text'
--
launchPermission :: LaunchPermission
launchPermission = LaunchPermission
    { _lpUserId = Nothing
    , _lpGroup  = Nothing
    }

-- | The name of the group.
lpGroup :: Lens' LaunchPermission (Maybe PermissionGroup)
lpGroup = lens _lpGroup (\s a -> s { _lpGroup = a })

-- | The AWS account ID.
lpUserId :: Lens' LaunchPermission (Maybe Text)
lpUserId = lens _lpUserId (\s a -> s { _lpUserId = a })

instance FromXML LaunchPermission where
    parseXML x = LaunchPermission
        <$> x .@? "group"
        <*> x .@? "userId"

instance ToQuery LaunchPermission where
    toQuery LaunchPermission{..} = mconcat
        [ "Group"  =? _lpGroup
        , "UserId" =? _lpUserId
        ]

data RouteState
    = RSActive    -- ^ active
    | RSBlackhole -- ^ blackhole
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable RouteState

instance FromText RouteState where
    parser = takeLowerText >>= \case
        "active"    -> pure RSActive
        "blackhole" -> pure RSBlackhole
        e           -> fail $
            "Failure parsing RouteState from " ++ show e

instance ToText RouteState where
    toText = \case
        RSActive    -> "active"
        RSBlackhole -> "blackhole"

instance ToByteString RouteState
instance ToHeader     RouteState
instance ToQuery      RouteState

instance FromXML RouteState where
    parseXML = parseXMLText "RouteState"

data RouteTableAssociation = RouteTableAssociation
    { _rtaMain                    :: Maybe Bool
    , _rtaRouteTableAssociationId :: Maybe Text
    , _rtaRouteTableId            :: Maybe Text
    , _rtaSubnetId                :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RouteTableAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtaMain' @::@ 'Maybe' 'Bool'
--
-- * 'rtaRouteTableAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'rtaRouteTableId' @::@ 'Maybe' 'Text'
--
-- * 'rtaSubnetId' @::@ 'Maybe' 'Text'
--
routeTableAssociation :: RouteTableAssociation
routeTableAssociation = RouteTableAssociation
    { _rtaRouteTableAssociationId = Nothing
    , _rtaRouteTableId            = Nothing
    , _rtaSubnetId                = Nothing
    , _rtaMain                    = Nothing
    }

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain = lens _rtaMain (\s a -> s { _rtaMain = a })

-- | The ID of the association between a route table and a subnet.
rtaRouteTableAssociationId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableAssociationId =
    lens _rtaRouteTableAssociationId
        (\s a -> s { _rtaRouteTableAssociationId = a })

-- | The ID of the route table.
rtaRouteTableId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableId = lens _rtaRouteTableId (\s a -> s { _rtaRouteTableId = a })

-- | The ID of the subnet.
rtaSubnetId :: Lens' RouteTableAssociation (Maybe Text)
rtaSubnetId = lens _rtaSubnetId (\s a -> s { _rtaSubnetId = a })

instance FromXML RouteTableAssociation where
    parseXML x = RouteTableAssociation
        <$> x .@? "main"
        <*> x .@? "routeTableAssociationId"
        <*> x .@? "routeTableId"
        <*> x .@? "subnetId"

instance ToQuery RouteTableAssociation where
    toQuery RouteTableAssociation{..} = mconcat
        [ "Main"                    =? _rtaMain
        , "RouteTableAssociationId" =? _rtaRouteTableAssociationId
        , "RouteTableId"            =? _rtaRouteTableId
        , "SubnetId"                =? _rtaSubnetId
        ]

data BundleTaskState
    = BTSBundling           -- ^ bundling
    | BTSCancelling         -- ^ cancelling
    | BTSComplete           -- ^ complete
    | BTSFailed             -- ^ failed
    | BTSPending            -- ^ pending
    | BTSStoring            -- ^ storing
    | BTSWaitingForShutdown -- ^ waiting-for-shutdown
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable BundleTaskState

instance FromText BundleTaskState where
    parser = takeLowerText >>= \case
        "bundling"             -> pure BTSBundling
        "cancelling"           -> pure BTSCancelling
        "complete"             -> pure BTSComplete
        "failed"               -> pure BTSFailed
        "pending"              -> pure BTSPending
        "storing"              -> pure BTSStoring
        "waiting-for-shutdown" -> pure BTSWaitingForShutdown
        e                      -> fail $
            "Failure parsing BundleTaskState from " ++ show e

instance ToText BundleTaskState where
    toText = \case
        BTSBundling           -> "bundling"
        BTSCancelling         -> "cancelling"
        BTSComplete           -> "complete"
        BTSFailed             -> "failed"
        BTSPending            -> "pending"
        BTSStoring            -> "storing"
        BTSWaitingForShutdown -> "waiting-for-shutdown"

instance ToByteString BundleTaskState
instance ToHeader     BundleTaskState
instance ToQuery      BundleTaskState

instance FromXML BundleTaskState where
    parseXML = parseXMLText "BundleTaskState"

data PortRange = PortRange
    { _prFrom :: Maybe Int
    , _prTo   :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'PortRange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prFrom' @::@ 'Maybe' 'Int'
--
-- * 'prTo' @::@ 'Maybe' 'Int'
--
portRange :: PortRange
portRange = PortRange
    { _prFrom = Nothing
    , _prTo   = Nothing
    }

-- | The first port in the range.
prFrom :: Lens' PortRange (Maybe Int)
prFrom = lens _prFrom (\s a -> s { _prFrom = a })

-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Int)
prTo = lens _prTo (\s a -> s { _prTo = a })

instance FromXML PortRange where
    parseXML x = PortRange
        <$> x .@? "from"
        <*> x .@? "to"

instance ToQuery PortRange where
    toQuery PortRange{..} = mconcat
        [ "From" =? _prFrom
        , "To"   =? _prTo
        ]

data VpcAttributeName
    = EnableDnsHostnames -- ^ enableDnsHostnames
    | EnableDnsSupport   -- ^ enableDnsSupport
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VpcAttributeName

instance FromText VpcAttributeName where
    parser = takeLowerText >>= \case
        "enablednshostnames" -> pure EnableDnsHostnames
        "enablednssupport"   -> pure EnableDnsSupport
        e                    -> fail $
            "Failure parsing VpcAttributeName from " ++ show e

instance ToText VpcAttributeName where
    toText = \case
        EnableDnsHostnames -> "enableDnsHostnames"
        EnableDnsSupport   -> "enableDnsSupport"

instance ToByteString VpcAttributeName
instance ToHeader     VpcAttributeName
instance ToQuery      VpcAttributeName

instance FromXML VpcAttributeName where
    parseXML = parseXMLText "VpcAttributeName"

data ReservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone :: Maybe Text
    , _ricInstanceCount    :: Maybe Int
    , _ricInstanceType     :: Maybe InstanceType
    , _ricPlatform         :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ReservedInstancesConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ricAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ricInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'ricInstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'ricPlatform' @::@ 'Maybe' 'Text'
--
reservedInstancesConfiguration :: ReservedInstancesConfiguration
reservedInstancesConfiguration = ReservedInstancesConfiguration
    { _ricAvailabilityZone = Nothing
    , _ricPlatform         = Nothing
    , _ricInstanceCount    = Nothing
    , _ricInstanceType     = Nothing
    }

-- | The Availability Zone for the modified Reserved Instances.
ricAvailabilityZone :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricAvailabilityZone =
    lens _ricAvailabilityZone (\s a -> s { _ricAvailabilityZone = a })

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Int)
ricInstanceCount = lens _ricInstanceCount (\s a -> s { _ricInstanceCount = a })

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe InstanceType)
ricInstanceType = lens _ricInstanceType (\s a -> s { _ricInstanceType = a })

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform = lens _ricPlatform (\s a -> s { _ricPlatform = a })

instance FromXML ReservedInstancesConfiguration where
    parseXML x = ReservedInstancesConfiguration
        <$> x .@? "availabilityZone"
        <*> x .@? "instanceCount"
        <*> x .@? "instanceType"
        <*> x .@? "platform"

instance ToQuery ReservedInstancesConfiguration where
    toQuery ReservedInstancesConfiguration{..} = mconcat
        [ "AvailabilityZone" =? _ricAvailabilityZone
        , "InstanceCount"    =? _ricInstanceCount
        , "InstanceType"     =? _ricInstanceType
        , "Platform"         =? _ricPlatform
        ]

data VolumeStatusDetails = VolumeStatusDetails
    { _vsdName   :: Maybe VolumeStatusName
    , _vsdStatus :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VolumeStatusDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsdName' @::@ 'Maybe' 'VolumeStatusName'
--
-- * 'vsdStatus' @::@ 'Maybe' 'Text'
--
volumeStatusDetails :: VolumeStatusDetails
volumeStatusDetails = VolumeStatusDetails
    { _vsdName   = Nothing
    , _vsdStatus = Nothing
    }

-- | The name of the volume status.
vsdName :: Lens' VolumeStatusDetails (Maybe VolumeStatusName)
vsdName = lens _vsdName (\s a -> s { _vsdName = a })

-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus = lens _vsdStatus (\s a -> s { _vsdStatus = a })

instance FromXML VolumeStatusDetails where
    parseXML x = VolumeStatusDetails
        <$> x .@? "name"
        <*> x .@? "status"

instance ToQuery VolumeStatusDetails where
    toQuery VolumeStatusDetails{..} = mconcat
        [ "Name"   =? _vsdName
        , "Status" =? _vsdStatus
        ]

data SpotInstanceState
    = SISActive    -- ^ active
    | SISCancelled -- ^ cancelled
    | SISClosed    -- ^ closed
    | SISFailed    -- ^ failed
    | SISOpen      -- ^ open
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SpotInstanceState

instance FromText SpotInstanceState where
    parser = takeLowerText >>= \case
        "active"    -> pure SISActive
        "cancelled" -> pure SISCancelled
        "closed"    -> pure SISClosed
        "failed"    -> pure SISFailed
        "open"      -> pure SISOpen
        e           -> fail $
            "Failure parsing SpotInstanceState from " ++ show e

instance ToText SpotInstanceState where
    toText = \case
        SISActive    -> "active"
        SISCancelled -> "cancelled"
        SISClosed    -> "closed"
        SISFailed    -> "failed"
        SISOpen      -> "open"

instance ToByteString SpotInstanceState
instance ToHeader     SpotInstanceState
instance ToQuery      SpotInstanceState

instance FromXML SpotInstanceState where
    parseXML = parseXMLText "SpotInstanceState"

newtype VpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'VpnConnectionOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcoStaticRoutesOnly' @::@ 'Maybe' 'Bool'
--
vpnConnectionOptions :: VpnConnectionOptions
vpnConnectionOptions = VpnConnectionOptions
    { _vcoStaticRoutesOnly = Nothing
    }

-- | Indicates whether the VPN connection uses static routes only. Static routes
-- must be used for devices that don't support BGP.
vcoStaticRoutesOnly :: Lens' VpnConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly =
    lens _vcoStaticRoutesOnly (\s a -> s { _vcoStaticRoutesOnly = a })

instance FromXML VpnConnectionOptions where
    parseXML x = VpnConnectionOptions
        <$> x .@? "staticRoutesOnly"

instance ToQuery VpnConnectionOptions where
    toQuery VpnConnectionOptions{..} = mconcat
        [ "StaticRoutesOnly" =? _vcoStaticRoutesOnly
        ]

data UserIdGroupPair = UserIdGroupPair
    { _uigpGroupId   :: Maybe Text
    , _uigpGroupName :: Maybe Text
    , _uigpUserId    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UserIdGroupPair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uigpGroupId' @::@ 'Maybe' 'Text'
--
-- * 'uigpGroupName' @::@ 'Maybe' 'Text'
--
-- * 'uigpUserId' @::@ 'Maybe' 'Text'
--
userIdGroupPair :: UserIdGroupPair
userIdGroupPair = UserIdGroupPair
    { _uigpUserId    = Nothing
    , _uigpGroupName = Nothing
    , _uigpGroupId   = Nothing
    }

-- | The ID of the security group.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId = lens _uigpGroupId (\s a -> s { _uigpGroupId = a })

-- | The name of the security group. In a request, use this parameter for a
-- security group in EC2-Classic or a default VPC only. For a security group in
-- a nondefault VPC, use 'GroupId'.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName = lens _uigpGroupName (\s a -> s { _uigpGroupName = a })

-- | The ID of an AWS account. EC2-Classic only.
uigpUserId :: Lens' UserIdGroupPair (Maybe Text)
uigpUserId = lens _uigpUserId (\s a -> s { _uigpUserId = a })

instance FromXML UserIdGroupPair where
    parseXML x = UserIdGroupPair
        <$> x .@? "groupId"
        <*> x .@? "groupName"
        <*> x .@? "userId"

instance ToQuery UserIdGroupPair where
    toQuery UserIdGroupPair{..} = mconcat
        [ "GroupId"   =? _uigpGroupId
        , "GroupName" =? _uigpGroupName
        , "UserId"    =? _uigpUserId
        ]

data InstanceStatusSummary = InstanceStatusSummary
    { _issDetails :: List "item" InstanceStatusDetails
    , _issStatus  :: Maybe SummaryStatus
    } deriving (Eq, Read, Show)

-- | 'InstanceStatusSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'issDetails' @::@ ['InstanceStatusDetails']
--
-- * 'issStatus' @::@ 'Maybe' 'SummaryStatus'
--
instanceStatusSummary :: InstanceStatusSummary
instanceStatusSummary = InstanceStatusSummary
    { _issStatus  = Nothing
    , _issDetails = mempty
    }

-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary [InstanceStatusDetails]
issDetails = lens _issDetails (\s a -> s { _issDetails = a }) . _List

-- | The status.
issStatus :: Lens' InstanceStatusSummary (Maybe SummaryStatus)
issStatus = lens _issStatus (\s a -> s { _issStatus = a })

instance FromXML InstanceStatusSummary where
    parseXML x = InstanceStatusSummary
        <$> x .@? "details" .!@ mempty
        <*> x .@? "status"

instance ToQuery InstanceStatusSummary where
    toQuery InstanceStatusSummary{..} = mconcat
        [ "Details" `toQueryList` _issDetails
        , "Status"  =? _issStatus
        ]

data SpotPlacement = SpotPlacement
    { _sp1AvailabilityZone :: Maybe Text
    , _sp1GroupName        :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SpotPlacement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sp1AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'sp1GroupName' @::@ 'Maybe' 'Text'
--
spotPlacement :: SpotPlacement
spotPlacement = SpotPlacement
    { _sp1AvailabilityZone = Nothing
    , _sp1GroupName        = Nothing
    }

-- | The Availability Zone.
sp1AvailabilityZone :: Lens' SpotPlacement (Maybe Text)
sp1AvailabilityZone =
    lens _sp1AvailabilityZone (\s a -> s { _sp1AvailabilityZone = a })

-- | The name of the placement group (for cluster instances).
sp1GroupName :: Lens' SpotPlacement (Maybe Text)
sp1GroupName = lens _sp1GroupName (\s a -> s { _sp1GroupName = a })

instance FromXML SpotPlacement where
    parseXML x = SpotPlacement
        <$> x .@? "availabilityZone"
        <*> x .@? "groupName"

instance ToQuery SpotPlacement where
    toQuery SpotPlacement{..} = mconcat
        [ "AvailabilityZone" =? _sp1AvailabilityZone
        , "GroupName"        =? _sp1GroupName
        ]

data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsDeleteOnTermination :: Maybe Bool
    , _eibdsVolumeId            :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'EbsInstanceBlockDeviceSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eibdsDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'eibdsVolumeId' @::@ 'Maybe' 'Text'
--
ebsInstanceBlockDeviceSpecification :: EbsInstanceBlockDeviceSpecification
ebsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { _eibdsVolumeId            = Nothing
    , _eibdsDeleteOnTermination = Nothing
    }

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination =
    lens _eibdsDeleteOnTermination
        (\s a -> s { _eibdsDeleteOnTermination = a })

-- | The ID of the EBS volume.
eibdsVolumeId :: Lens' EbsInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId = lens _eibdsVolumeId (\s a -> s { _eibdsVolumeId = a })

instance FromXML EbsInstanceBlockDeviceSpecification where
    parseXML x = EbsInstanceBlockDeviceSpecification
        <$> x .@? "deleteOnTermination"
        <*> x .@? "volumeId"

instance ToQuery EbsInstanceBlockDeviceSpecification where
    toQuery EbsInstanceBlockDeviceSpecification{..} = mconcat
        [ "DeleteOnTermination" =? _eibdsDeleteOnTermination
        , "VolumeId"            =? _eibdsVolumeId
        ]

data NetworkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId :: Maybe Text
    , _naaNetworkAclId            :: Maybe Text
    , _naaSubnetId                :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'NetworkAclAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naaNetworkAclAssociationId' @::@ 'Maybe' 'Text'
--
-- * 'naaNetworkAclId' @::@ 'Maybe' 'Text'
--
-- * 'naaSubnetId' @::@ 'Maybe' 'Text'
--
networkAclAssociation :: NetworkAclAssociation
networkAclAssociation = NetworkAclAssociation
    { _naaNetworkAclAssociationId = Nothing
    , _naaNetworkAclId            = Nothing
    , _naaSubnetId                = Nothing
    }

-- | The ID of the association between a network ACL and a subnet.
naaNetworkAclAssociationId :: Lens' NetworkAclAssociation (Maybe Text)
naaNetworkAclAssociationId =
    lens _naaNetworkAclAssociationId
        (\s a -> s { _naaNetworkAclAssociationId = a })

-- | The ID of the network ACL.
naaNetworkAclId :: Lens' NetworkAclAssociation (Maybe Text)
naaNetworkAclId = lens _naaNetworkAclId (\s a -> s { _naaNetworkAclId = a })

-- | The ID of the subnet.
naaSubnetId :: Lens' NetworkAclAssociation (Maybe Text)
naaSubnetId = lens _naaSubnetId (\s a -> s { _naaSubnetId = a })

instance FromXML NetworkAclAssociation where
    parseXML x = NetworkAclAssociation
        <$> x .@? "networkAclAssociationId"
        <*> x .@? "networkAclId"
        <*> x .@? "subnetId"

instance ToQuery NetworkAclAssociation where
    toQuery NetworkAclAssociation{..} = mconcat
        [ "NetworkAclAssociationId" =? _naaNetworkAclAssociationId
        , "NetworkAclId"            =? _naaNetworkAclId
        , "SubnetId"                =? _naaSubnetId
        ]

data BundleTask = BundleTask
    { _btBundleId        :: Text
    , _btBundleTaskError :: Maybe BundleTaskError
    , _btInstanceId      :: Text
    , _btProgress        :: Text
    , _btStartTime       :: ISO8601
    , _btState           :: BundleTaskState
    , _btStorage         :: Storage
    , _btUpdateTime      :: ISO8601
    } deriving (Eq, Read, Show)

-- | 'BundleTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'btBundleId' @::@ 'Text'
--
-- * 'btBundleTaskError' @::@ 'Maybe' 'BundleTaskError'
--
-- * 'btInstanceId' @::@ 'Text'
--
-- * 'btProgress' @::@ 'Text'
--
-- * 'btStartTime' @::@ 'UTCTime'
--
-- * 'btState' @::@ 'BundleTaskState'
--
-- * 'btStorage' @::@ 'Storage'
--
-- * 'btUpdateTime' @::@ 'UTCTime'
--
bundleTask :: Text -- ^ 'btInstanceId'
           -> Text -- ^ 'btBundleId'
           -> BundleTaskState -- ^ 'btState'
           -> UTCTime -- ^ 'btStartTime'
           -> UTCTime -- ^ 'btUpdateTime'
           -> Storage -- ^ 'btStorage'
           -> Text -- ^ 'btProgress'
           -> BundleTask
bundleTask p1 p2 p3 p4 p5 p6 p7 = BundleTask
    { _btInstanceId      = p1
    , _btBundleId        = p2
    , _btState           = p3
    , _btStartTime       = withIso _Time (const id) p4
    , _btUpdateTime      = withIso _Time (const id) p5
    , _btStorage         = p6
    , _btProgress        = p7
    , _btBundleTaskError = Nothing
    }

-- | The ID of the bundle task.
btBundleId :: Lens' BundleTask Text
btBundleId = lens _btBundleId (\s a -> s { _btBundleId = a })

-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError =
    lens _btBundleTaskError (\s a -> s { _btBundleTaskError = a })

-- | The ID of the instance associated with this bundle task.
btInstanceId :: Lens' BundleTask Text
btInstanceId = lens _btInstanceId (\s a -> s { _btInstanceId = a })

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask Text
btProgress = lens _btProgress (\s a -> s { _btProgress = a })

-- | The time this task started.
btStartTime :: Lens' BundleTask UTCTime
btStartTime = lens _btStartTime (\s a -> s { _btStartTime = a }) . _Time

-- | The state of the task.
btState :: Lens' BundleTask BundleTaskState
btState = lens _btState (\s a -> s { _btState = a })

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask Storage
btStorage = lens _btStorage (\s a -> s { _btStorage = a })

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask UTCTime
btUpdateTime = lens _btUpdateTime (\s a -> s { _btUpdateTime = a }) . _Time

instance FromXML BundleTask where
    parseXML x = BundleTask
        <$> x .@  "bundleId"
        <*> x .@? "error"
        <*> x .@  "instanceId"
        <*> x .@  "progress"
        <*> x .@  "startTime"
        <*> x .@  "state"
        <*> x .@  "storage"
        <*> x .@  "updateTime"

instance ToQuery BundleTask where
    toQuery BundleTask{..} = mconcat
        [ "BundleId"   =? _btBundleId
        , "Error"      =? _btBundleTaskError
        , "InstanceId" =? _btInstanceId
        , "Progress"   =? _btProgress
        , "StartTime"  =? _btStartTime
        , "State"      =? _btState
        , "Storage"    =? _btStorage
        , "UpdateTime" =? _btUpdateTime
        ]

data InstanceStatusEvent = InstanceStatusEvent
    { _iseCode        :: Maybe EventCode
    , _iseDescription :: Maybe Text
    , _iseNotAfter    :: Maybe ISO8601
    , _iseNotBefore   :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'InstanceStatusEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iseCode' @::@ 'Maybe' 'EventCode'
--
-- * 'iseDescription' @::@ 'Maybe' 'Text'
--
-- * 'iseNotAfter' @::@ 'Maybe' 'UTCTime'
--
-- * 'iseNotBefore' @::@ 'Maybe' 'UTCTime'
--
instanceStatusEvent :: InstanceStatusEvent
instanceStatusEvent = InstanceStatusEvent
    { _iseCode        = Nothing
    , _iseDescription = Nothing
    , _iseNotBefore   = Nothing
    , _iseNotAfter    = Nothing
    }

-- | The event code.
iseCode :: Lens' InstanceStatusEvent (Maybe EventCode)
iseCode = lens _iseCode (\s a -> s { _iseCode = a })

-- | A description of the event.
--
-- After a scheduled event is completed, it can still be described for up to a
-- week. If the event has been completed, this description starts with the
-- following text: [Completed].
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription = lens _iseDescription (\s a -> s { _iseDescription = a })

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotAfter = lens _iseNotAfter (\s a -> s { _iseNotAfter = a }) . mapping _Time

-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotBefore = lens _iseNotBefore (\s a -> s { _iseNotBefore = a }) . mapping _Time

instance FromXML InstanceStatusEvent where
    parseXML x = InstanceStatusEvent
        <$> x .@? "code"
        <*> x .@? "description"
        <*> x .@? "notAfter"
        <*> x .@? "notBefore"

instance ToQuery InstanceStatusEvent where
    toQuery InstanceStatusEvent{..} = mconcat
        [ "Code"        =? _iseCode
        , "Description" =? _iseDescription
        , "NotAfter"    =? _iseNotAfter
        , "NotBefore"   =? _iseNotBefore
        ]

data InstanceType
    = C1_Medium   -- ^ c1.medium
    | C1_XLarge   -- ^ c1.xlarge
    | C3_2XLarge  -- ^ c3.2xlarge
    | C3_4XLarge  -- ^ c3.4xlarge
    | C3_8XLarge  -- ^ c3.8xlarge
    | C3_Large    -- ^ c3.large
    | C3_XLarge   -- ^ c3.xlarge
    | C4_2XLarge  -- ^ c4.2xlarge
    | C4_4XLarge  -- ^ c4.4xlarge
    | C4_8XLarge  -- ^ c4.8xlarge
    | C4_Large    -- ^ c4.large
    | C4_XLarge   -- ^ c4.xlarge
    | CC1_4XLarge -- ^ cc1.4xlarge
    | CC2_8XLarge -- ^ cc2.8xlarge
    | CG1_4XLarge -- ^ cg1.4xlarge
    | CR1_8XLarge -- ^ cr1.8xlarge
    | D2_2XLarge  -- ^ d2.2xlarge
    | D2_4XLarge  -- ^ d2.4xlarge
    | D2_8XLarge  -- ^ d2.8xlarge
    | D2_XLarge   -- ^ d2.xlarge
    | G2_2XLarge  -- ^ g2.2xlarge
    | HI1_4XLarge -- ^ hi1.4xlarge
    | HS1_8XLarge -- ^ hs1.8xlarge
    | I2_2XLarge  -- ^ i2.2xlarge
    | I2_4XLarge  -- ^ i2.4xlarge
    | I2_8XLarge  -- ^ i2.8xlarge
    | I2_XLarge   -- ^ i2.xlarge
    | M1_Large    -- ^ m1.large
    | M1_Medium   -- ^ m1.medium
    | M1_Small    -- ^ m1.small
    | M1_XLarge   -- ^ m1.xlarge
    | M2_2XLarge  -- ^ m2.2xlarge
    | M2_4XLarge  -- ^ m2.4xlarge
    | M2_XLarge   -- ^ m2.xlarge
    | M3_2XLarge  -- ^ m3.2xlarge
    | M3_Large    -- ^ m3.large
    | M3_Medium   -- ^ m3.medium
    | M3_XLarge   -- ^ m3.xlarge
    | R3_2XLarge  -- ^ r3.2xlarge
    | R3_4XLarge  -- ^ r3.4xlarge
    | R3_8XLarge  -- ^ r3.8xlarge
    | R3_Large    -- ^ r3.large
    | R3_XLarge   -- ^ r3.xlarge
    | T1_Micro    -- ^ t1.micro
    | T2_Large    -- ^ t2.large
    | T2_Medium   -- ^ t2.medium
    | T2_Micro    -- ^ t2.micro
    | T2_Small    -- ^ t2.small
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable InstanceType

instance FromText InstanceType where
    parser = takeLowerText >>= \case
        "c1.medium"   -> pure C1_Medium
        "c1.xlarge"   -> pure C1_XLarge
        "c3.2xlarge"  -> pure C3_2XLarge
        "c3.4xlarge"  -> pure C3_4XLarge
        "c3.8xlarge"  -> pure C3_8XLarge
        "c3.large"    -> pure C3_Large
        "c3.xlarge"   -> pure C3_XLarge
        "c4.2xlarge"  -> pure C4_2XLarge
        "c4.4xlarge"  -> pure C4_4XLarge
        "c4.8xlarge"  -> pure C4_8XLarge
        "c4.large"    -> pure C4_Large
        "c4.xlarge"   -> pure C4_XLarge
        "cc1.4xlarge" -> pure CC1_4XLarge
        "cc2.8xlarge" -> pure CC2_8XLarge
        "cg1.4xlarge" -> pure CG1_4XLarge
        "cr1.8xlarge" -> pure CR1_8XLarge
        "d2.2xlarge"  -> pure D2_2XLarge
        "d2.4xlarge"  -> pure D2_4XLarge
        "d2.8xlarge"  -> pure D2_8XLarge
        "d2.xlarge"   -> pure D2_XLarge
        "g2.2xlarge"  -> pure G2_2XLarge
        "hi1.4xlarge" -> pure HI1_4XLarge
        "hs1.8xlarge" -> pure HS1_8XLarge
        "i2.2xlarge"  -> pure I2_2XLarge
        "i2.4xlarge"  -> pure I2_4XLarge
        "i2.8xlarge"  -> pure I2_8XLarge
        "i2.xlarge"   -> pure I2_XLarge
        "m1.large"    -> pure M1_Large
        "m1.medium"   -> pure M1_Medium
        "m1.small"    -> pure M1_Small
        "m1.xlarge"   -> pure M1_XLarge
        "m2.2xlarge"  -> pure M2_2XLarge
        "m2.4xlarge"  -> pure M2_4XLarge
        "m2.xlarge"   -> pure M2_XLarge
        "m3.2xlarge"  -> pure M3_2XLarge
        "m3.large"    -> pure M3_Large
        "m3.medium"   -> pure M3_Medium
        "m3.xlarge"   -> pure M3_XLarge
        "r3.2xlarge"  -> pure R3_2XLarge
        "r3.4xlarge"  -> pure R3_4XLarge
        "r3.8xlarge"  -> pure R3_8XLarge
        "r3.large"    -> pure R3_Large
        "r3.xlarge"   -> pure R3_XLarge
        "t1.micro"    -> pure T1_Micro
        "t2.large"    -> pure T2_Large
        "t2.medium"   -> pure T2_Medium
        "t2.micro"    -> pure T2_Micro
        "t2.small"    -> pure T2_Small
        e             -> fail $
            "Failure parsing InstanceType from " ++ show e

instance ToText InstanceType where
    toText = \case
        C1_Medium   -> "c1.medium"
        C1_XLarge   -> "c1.xlarge"
        C3_2XLarge  -> "c3.2xlarge"
        C3_4XLarge  -> "c3.4xlarge"
        C3_8XLarge  -> "c3.8xlarge"
        C3_Large    -> "c3.large"
        C3_XLarge   -> "c3.xlarge"
        C4_2XLarge  -> "c4.2xlarge"
        C4_4XLarge  -> "c4.4xlarge"
        C4_8XLarge  -> "c4.8xlarge"
        C4_Large    -> "c4.large"
        C4_XLarge   -> "c4.xlarge"
        CC1_4XLarge -> "cc1.4xlarge"
        CC2_8XLarge -> "cc2.8xlarge"
        CG1_4XLarge -> "cg1.4xlarge"
        CR1_8XLarge -> "cr1.8xlarge"
        D2_2XLarge  -> "d2.2xlarge"
        D2_4XLarge  -> "d2.4xlarge"
        D2_8XLarge  -> "d2.8xlarge"
        D2_XLarge   -> "d2.xlarge"
        G2_2XLarge  -> "g2.2xlarge"
        HI1_4XLarge -> "hi1.4xlarge"
        HS1_8XLarge -> "hs1.8xlarge"
        I2_2XLarge  -> "i2.2xlarge"
        I2_4XLarge  -> "i2.4xlarge"
        I2_8XLarge  -> "i2.8xlarge"
        I2_XLarge   -> "i2.xlarge"
        M1_Large    -> "m1.large"
        M1_Medium   -> "m1.medium"
        M1_Small    -> "m1.small"
        M1_XLarge   -> "m1.xlarge"
        M2_2XLarge  -> "m2.2xlarge"
        M2_4XLarge  -> "m2.4xlarge"
        M2_XLarge   -> "m2.xlarge"
        M3_2XLarge  -> "m3.2xlarge"
        M3_Large    -> "m3.large"
        M3_Medium   -> "m3.medium"
        M3_XLarge   -> "m3.xlarge"
        R3_2XLarge  -> "r3.2xlarge"
        R3_4XLarge  -> "r3.4xlarge"
        R3_8XLarge  -> "r3.8xlarge"
        R3_Large    -> "r3.large"
        R3_XLarge   -> "r3.xlarge"
        T1_Micro    -> "t1.micro"
        T2_Large    -> "t2.large"
        T2_Medium   -> "t2.medium"
        T2_Micro    -> "t2.micro"
        T2_Small    -> "t2.small"

instance ToByteString InstanceType
instance ToHeader     InstanceType
instance ToQuery      InstanceType

instance FromXML InstanceType where
    parseXML = parseXMLText "InstanceType"

data Route = Route
    { _rDestinationCidrBlock    :: Maybe Text
    , _rDestinationPrefixListId :: Maybe Text
    , _rGatewayId               :: Maybe Text
    , _rInstanceId              :: Maybe Text
    , _rInstanceOwnerId         :: Maybe Text
    , _rNetworkInterfaceId      :: Maybe Text
    , _rOrigin                  :: Maybe RouteOrigin
    , _rState                   :: Maybe RouteState
    , _rVpcPeeringConnectionId  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Route' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rDestinationCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'rDestinationPrefixListId' @::@ 'Maybe' 'Text'
--
-- * 'rGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'rInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'rInstanceOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'rNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'rOrigin' @::@ 'Maybe' 'RouteOrigin'
--
-- * 'rState' @::@ 'Maybe' 'RouteState'
--
-- * 'rVpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
route :: Route
route = Route
    { _rDestinationCidrBlock    = Nothing
    , _rDestinationPrefixListId = Nothing
    , _rGatewayId               = Nothing
    , _rInstanceId              = Nothing
    , _rInstanceOwnerId         = Nothing
    , _rNetworkInterfaceId      = Nothing
    , _rVpcPeeringConnectionId  = Nothing
    , _rState                   = Nothing
    , _rOrigin                  = Nothing
    }

-- | The CIDR block used for the destination match.
rDestinationCidrBlock :: Lens' Route (Maybe Text)
rDestinationCidrBlock =
    lens _rDestinationCidrBlock (\s a -> s { _rDestinationCidrBlock = a })

-- | The prefix of the AWS service.
rDestinationPrefixListId :: Lens' Route (Maybe Text)
rDestinationPrefixListId =
    lens _rDestinationPrefixListId
        (\s a -> s { _rDestinationPrefixListId = a })

-- | The ID of a gateway attached to your VPC.
rGatewayId :: Lens' Route (Maybe Text)
rGatewayId = lens _rGatewayId (\s a -> s { _rGatewayId = a })

-- | The ID of a NAT instance in your VPC.
rInstanceId :: Lens' Route (Maybe Text)
rInstanceId = lens _rInstanceId (\s a -> s { _rInstanceId = a })

-- | The AWS account ID of the owner of the instance.
rInstanceOwnerId :: Lens' Route (Maybe Text)
rInstanceOwnerId = lens _rInstanceOwnerId (\s a -> s { _rInstanceOwnerId = a })

-- | The ID of the network interface.
rNetworkInterfaceId :: Lens' Route (Maybe Text)
rNetworkInterfaceId =
    lens _rNetworkInterfaceId (\s a -> s { _rNetworkInterfaceId = a })

-- | Describes how the route was created.
--
-- 'CreateRouteTable' indicates that route was automatically created when the
-- route table was created.  'CreateRoute' indicates that the route was manually
-- added to the route table.  'EnableVgwRoutePropagation' indicates that the route
-- was propagated by route propagation.
rOrigin :: Lens' Route (Maybe RouteOrigin)
rOrigin = lens _rOrigin (\s a -> s { _rOrigin = a })

-- | The state of the route. The 'blackhole' state indicates that the route's target
-- isn't available (for example, the specified gateway isn't attached to the
-- VPC, or the specified NAT instance has been terminated).
rState :: Lens' Route (Maybe RouteState)
rState = lens _rState (\s a -> s { _rState = a })

-- | The ID of the VPC peering connection.
rVpcPeeringConnectionId :: Lens' Route (Maybe Text)
rVpcPeeringConnectionId =
    lens _rVpcPeeringConnectionId (\s a -> s { _rVpcPeeringConnectionId = a })

instance FromXML Route where
    parseXML x = Route
        <$> x .@? "destinationCidrBlock"
        <*> x .@? "destinationPrefixListId"
        <*> x .@? "gatewayId"
        <*> x .@? "instanceId"
        <*> x .@? "instanceOwnerId"
        <*> x .@? "networkInterfaceId"
        <*> x .@? "origin"
        <*> x .@? "state"
        <*> x .@? "vpcPeeringConnectionId"

instance ToQuery Route where
    toQuery Route{..} = mconcat
        [ "DestinationCidrBlock"    =? _rDestinationCidrBlock
        , "DestinationPrefixListId" =? _rDestinationPrefixListId
        , "GatewayId"               =? _rGatewayId
        , "InstanceId"              =? _rInstanceId
        , "InstanceOwnerId"         =? _rInstanceOwnerId
        , "NetworkInterfaceId"      =? _rNetworkInterfaceId
        , "Origin"                  =? _rOrigin
        , "State"                   =? _rState
        , "VpcPeeringConnectionId"  =? _rVpcPeeringConnectionId
        ]

data SpotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsBucket  :: Maybe Text
    , _sdsFault   :: Maybe SpotInstanceStateFault
    , _sdsOwnerId :: Maybe Text
    , _sdsPrefix  :: Maybe Text
    , _sdsState   :: Maybe DatafeedSubscriptionState
    } deriving (Eq, Read, Show)

-- | 'SpotDatafeedSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdsBucket' @::@ 'Maybe' 'Text'
--
-- * 'sdsFault' @::@ 'Maybe' 'SpotInstanceStateFault'
--
-- * 'sdsOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'sdsPrefix' @::@ 'Maybe' 'Text'
--
-- * 'sdsState' @::@ 'Maybe' 'DatafeedSubscriptionState'
--
spotDatafeedSubscription :: SpotDatafeedSubscription
spotDatafeedSubscription = SpotDatafeedSubscription
    { _sdsOwnerId = Nothing
    , _sdsBucket  = Nothing
    , _sdsPrefix  = Nothing
    , _sdsState   = Nothing
    , _sdsFault   = Nothing
    }

-- | The Amazon S3 bucket where the Spot Instance data feed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket = lens _sdsBucket (\s a -> s { _sdsBucket = a })

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault = lens _sdsFault (\s a -> s { _sdsFault = a })

-- | The AWS account ID of the account.
sdsOwnerId :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsOwnerId = lens _sdsOwnerId (\s a -> s { _sdsOwnerId = a })

-- | The prefix that is prepended to data feed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix = lens _sdsPrefix (\s a -> s { _sdsPrefix = a })

-- | The state of the Spot Instance data feed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe DatafeedSubscriptionState)
sdsState = lens _sdsState (\s a -> s { _sdsState = a })

instance FromXML SpotDatafeedSubscription where
    parseXML x = SpotDatafeedSubscription
        <$> x .@? "bucket"
        <*> x .@? "fault"
        <*> x .@? "ownerId"
        <*> x .@? "prefix"
        <*> x .@? "state"

instance ToQuery SpotDatafeedSubscription where
    toQuery SpotDatafeedSubscription{..} = mconcat
        [ "Bucket"  =? _sdsBucket
        , "Fault"   =? _sdsFault
        , "OwnerId" =? _sdsOwnerId
        , "Prefix"  =? _sdsPrefix
        , "State"   =? _sdsState
        ]

newtype Storage = Storage
    { _sS3 :: Maybe S3Storage
    } deriving (Eq, Read, Show)

-- | 'Storage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sS3' @::@ 'Maybe' 'S3Storage'
--
storage :: Storage
storage = Storage
    { _sS3 = Nothing
    }

-- | An Amazon S3 storage location.
sS3 :: Lens' Storage (Maybe S3Storage)
sS3 = lens _sS3 (\s a -> s { _sS3 = a })

instance FromXML Storage where
    parseXML x = Storage
        <$> x .@? "S3"

instance ToQuery Storage where
    toQuery Storage{..} = mconcat
        [ "S3" =? _sS3
        ]

data SecurityGroup = SecurityGroup
    { _sgDescription         :: Text
    , _sgGroupId             :: Text
    , _sgGroupName           :: Text
    , _sgIpPermissions       :: List "item" IpPermission
    , _sgIpPermissionsEgress :: List "item" IpPermission
    , _sgOwnerId             :: Text
    , _sgTags                :: List "item" Tag
    , _sgVpcId               :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'SecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgDescription' @::@ 'Text'
--
-- * 'sgGroupId' @::@ 'Text'
--
-- * 'sgGroupName' @::@ 'Text'
--
-- * 'sgIpPermissions' @::@ ['IpPermission']
--
-- * 'sgIpPermissionsEgress' @::@ ['IpPermission']
--
-- * 'sgOwnerId' @::@ 'Text'
--
-- * 'sgTags' @::@ ['Tag']
--
-- * 'sgVpcId' @::@ 'Maybe' 'Text'
--
securityGroup :: Text -- ^ 'sgOwnerId'
              -> Text -- ^ 'sgGroupName'
              -> Text -- ^ 'sgGroupId'
              -> Text -- ^ 'sgDescription'
              -> SecurityGroup
securityGroup p1 p2 p3 p4 = SecurityGroup
    { _sgOwnerId             = p1
    , _sgGroupName           = p2
    , _sgGroupId             = p3
    , _sgDescription         = p4
    , _sgIpPermissions       = mempty
    , _sgIpPermissionsEgress = mempty
    , _sgVpcId               = Nothing
    , _sgTags                = mempty
    }

-- | A description of the security group.
sgDescription :: Lens' SecurityGroup Text
sgDescription = lens _sgDescription (\s a -> s { _sgDescription = a })

-- | The ID of the security group.
sgGroupId :: Lens' SecurityGroup Text
sgGroupId = lens _sgGroupId (\s a -> s { _sgGroupId = a })

-- | The name of the security group.
sgGroupName :: Lens' SecurityGroup Text
sgGroupName = lens _sgGroupName (\s a -> s { _sgGroupName = a })

-- | One or more inbound rules associated with the security group.
sgIpPermissions :: Lens' SecurityGroup [IpPermission]
sgIpPermissions = lens _sgIpPermissions (\s a -> s { _sgIpPermissions = a }) . _List

-- | [EC2-VPC] One or more outbound rules associated with the security group.
sgIpPermissionsEgress :: Lens' SecurityGroup [IpPermission]
sgIpPermissionsEgress =
    lens _sgIpPermissionsEgress (\s a -> s { _sgIpPermissionsEgress = a })
        . _List

-- | The AWS account ID of the owner of the security group.
sgOwnerId :: Lens' SecurityGroup Text
sgOwnerId = lens _sgOwnerId (\s a -> s { _sgOwnerId = a })

-- | Any tags assigned to the security group.
sgTags :: Lens' SecurityGroup [Tag]
sgTags = lens _sgTags (\s a -> s { _sgTags = a }) . _List

-- | [EC2-VPC] The ID of the VPC for the security group.
sgVpcId :: Lens' SecurityGroup (Maybe Text)
sgVpcId = lens _sgVpcId (\s a -> s { _sgVpcId = a })

instance FromXML SecurityGroup where
    parseXML x = SecurityGroup
        <$> x .@  "groupDescription"
        <*> x .@  "groupId"
        <*> x .@  "groupName"
        <*> x .@? "ipPermissions" .!@ mempty
        <*> x .@? "ipPermissionsEgress" .!@ mempty
        <*> x .@  "ownerId"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "vpcId"

instance ToQuery SecurityGroup where
    toQuery SecurityGroup{..} = mconcat
        [ "GroupDescription"    =? _sgDescription
        , "GroupId"             =? _sgGroupId
        , "GroupName"           =? _sgGroupName
        , "IpPermissions"       `toQueryList` _sgIpPermissions
        , "IpPermissionsEgress" `toQueryList` _sgIpPermissionsEgress
        , "OwnerId"             =? _sgOwnerId
        , "TagSet"              `toQueryList` _sgTags
        , "VpcId"               =? _sgVpcId
        ]

data CancelSpotInstanceRequestState
    = CSIRSActive    -- ^ active
    | CSIRSCancelled -- ^ cancelled
    | CSIRSClosed    -- ^ closed
    | CSIRSCompleted -- ^ completed
    | CSIRSOpen      -- ^ open
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable CancelSpotInstanceRequestState

instance FromText CancelSpotInstanceRequestState where
    parser = takeLowerText >>= \case
        "active"    -> pure CSIRSActive
        "cancelled" -> pure CSIRSCancelled
        "closed"    -> pure CSIRSClosed
        "completed" -> pure CSIRSCompleted
        "open"      -> pure CSIRSOpen
        e           -> fail $
            "Failure parsing CancelSpotInstanceRequestState from " ++ show e

instance ToText CancelSpotInstanceRequestState where
    toText = \case
        CSIRSActive    -> "active"
        CSIRSCancelled -> "cancelled"
        CSIRSClosed    -> "closed"
        CSIRSCompleted -> "completed"
        CSIRSOpen      -> "open"

instance ToByteString CancelSpotInstanceRequestState
instance ToHeader     CancelSpotInstanceRequestState
instance ToQuery      CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
    parseXML = parseXMLText "CancelSpotInstanceRequestState"

data PlacementGroupState
    = PGSAvailable -- ^ available
    | PGSDeleted   -- ^ deleted
    | PGSDeleting  -- ^ deleting
    | PGSPending   -- ^ pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable PlacementGroupState

instance FromText PlacementGroupState where
    parser = takeLowerText >>= \case
        "available" -> pure PGSAvailable
        "deleted"   -> pure PGSDeleted
        "deleting"  -> pure PGSDeleting
        "pending"   -> pure PGSPending
        e           -> fail $
            "Failure parsing PlacementGroupState from " ++ show e

instance ToText PlacementGroupState where
    toText = \case
        PGSAvailable -> "available"
        PGSDeleted   -> "deleted"
        PGSDeleting  -> "deleting"
        PGSPending   -> "pending"

instance ToByteString PlacementGroupState
instance ToHeader     PlacementGroupState
instance ToQuery      PlacementGroupState

instance FromXML PlacementGroupState where
    parseXML = parseXMLText "PlacementGroupState"

data ReservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId :: Maybe Text
    , _rimrTargetConfiguration :: Maybe ReservedInstancesConfiguration
    } deriving (Eq, Read, Show)

-- | 'ReservedInstancesModificationResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rimrReservedInstancesId' @::@ 'Maybe' 'Text'
--
-- * 'rimrTargetConfiguration' @::@ 'Maybe' 'ReservedInstancesConfiguration'
--
reservedInstancesModificationResult :: ReservedInstancesModificationResult
reservedInstancesModificationResult = ReservedInstancesModificationResult
    { _rimrReservedInstancesId = Nothing
    , _rimrTargetConfiguration = Nothing
    }

-- | The ID for the Reserved Instances that were created as part of the
-- modification request. This field is only available when the modification is
-- fulfilled.
rimrReservedInstancesId :: Lens' ReservedInstancesModificationResult (Maybe Text)
rimrReservedInstancesId =
    lens _rimrReservedInstancesId (\s a -> s { _rimrReservedInstancesId = a })

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration =
    lens _rimrTargetConfiguration (\s a -> s { _rimrTargetConfiguration = a })

instance FromXML ReservedInstancesModificationResult where
    parseXML x = ReservedInstancesModificationResult
        <$> x .@? "reservedInstancesId"
        <*> x .@? "targetConfiguration"

instance ToQuery ReservedInstancesModificationResult where
    toQuery ReservedInstancesModificationResult{..} = mconcat
        [ "ReservedInstancesId" =? _rimrReservedInstancesId
        , "TargetConfiguration" =? _rimrTargetConfiguration
        ]

data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName  :: Maybe Text
    , _ibdmsEbs         :: Maybe EbsInstanceBlockDeviceSpecification
    , _ibdmsNoDevice    :: Maybe Text
    , _ibdmsVirtualName :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'InstanceBlockDeviceMappingSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibdmsDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'ibdmsEbs' @::@ 'Maybe' 'EbsInstanceBlockDeviceSpecification'
--
-- * 'ibdmsNoDevice' @::@ 'Maybe' 'Text'
--
-- * 'ibdmsVirtualName' @::@ 'Maybe' 'Text'
--
instanceBlockDeviceMappingSpecification :: InstanceBlockDeviceMappingSpecification
instanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { _ibdmsDeviceName  = Nothing
    , _ibdmsEbs         = Nothing
    , _ibdmsVirtualName = Nothing
    , _ibdmsNoDevice    = Nothing
    }

-- | The device name exposed to the instance (for example, '/dev/sdh' or 'xvdh').
ibdmsDeviceName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsDeviceName = lens _ibdmsDeviceName (\s a -> s { _ibdmsDeviceName = a })

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
ibdmsEbs :: Lens' InstanceBlockDeviceMappingSpecification (Maybe EbsInstanceBlockDeviceSpecification)
ibdmsEbs = lens _ibdmsEbs (\s a -> s { _ibdmsEbs = a })

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsNoDevice = lens _ibdmsNoDevice (\s a -> s { _ibdmsNoDevice = a })

-- | The virtual device name.
ibdmsVirtualName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsVirtualName = lens _ibdmsVirtualName (\s a -> s { _ibdmsVirtualName = a })

instance FromXML InstanceBlockDeviceMappingSpecification where
    parseXML x = InstanceBlockDeviceMappingSpecification
        <$> x .@? "deviceName"
        <*> x .@? "ebs"
        <*> x .@? "noDevice"
        <*> x .@? "virtualName"

instance ToQuery InstanceBlockDeviceMappingSpecification where
    toQuery InstanceBlockDeviceMappingSpecification{..} = mconcat
        [ "DeviceName"  =? _ibdmsDeviceName
        , "Ebs"         =? _ibdmsEbs
        , "NoDevice"    =? _ibdmsNoDevice
        , "VirtualName" =? _ibdmsVirtualName
        ]

data ExportEnvironment
    = Citrix    -- ^ citrix
    | Microsoft -- ^ microsoft
    | Vmware    -- ^ vmware
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ExportEnvironment

instance FromText ExportEnvironment where
    parser = takeLowerText >>= \case
        "citrix"    -> pure Citrix
        "microsoft" -> pure Microsoft
        "vmware"    -> pure Vmware
        e           -> fail $
            "Failure parsing ExportEnvironment from " ++ show e

instance ToText ExportEnvironment where
    toText = \case
        Citrix    -> "citrix"
        Microsoft -> "microsoft"
        Vmware    -> "vmware"

instance ToByteString ExportEnvironment
instance ToHeader     ExportEnvironment
instance ToQuery      ExportEnvironment

instance FromXML ExportEnvironment where
    parseXML = parseXMLText "ExportEnvironment"

newtype UserData = UserData
    { _udData :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'UserData' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udData' @::@ 'Maybe' 'Text'
--
userData :: UserData
userData = UserData
    { _udData = Nothing
    }

-- | The Base64-encoded MIME user data for the instance.
udData :: Lens' UserData (Maybe Text)
udData = lens _udData (\s a -> s { _udData = a })

instance FromXML UserData where
    parseXML x = UserData
        <$> x .@? "data"

instance ToQuery UserData where
    toQuery UserData{..} = mconcat
        [ "Data" =? _udData
        ]

data VolumeAttachment = VolumeAttachment
    { _vaAttachTime          :: Maybe ISO8601
    , _vaDeleteOnTermination :: Maybe Bool
    , _vaDevice              :: Maybe Text
    , _vaInstanceId          :: Maybe Text
    , _vaState               :: Maybe VolumeAttachmentState
    , _vaVolumeId            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VolumeAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vaAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'vaDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'vaDevice' @::@ 'Maybe' 'Text'
--
-- * 'vaInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'vaState' @::@ 'Maybe' 'VolumeAttachmentState'
--
-- * 'vaVolumeId' @::@ 'Maybe' 'Text'
--
volumeAttachment :: VolumeAttachment
volumeAttachment = VolumeAttachment
    { _vaVolumeId            = Nothing
    , _vaInstanceId          = Nothing
    , _vaDevice              = Nothing
    , _vaState               = Nothing
    , _vaAttachTime          = Nothing
    , _vaDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
vaAttachTime :: Lens' VolumeAttachment (Maybe UTCTime)
vaAttachTime = lens _vaAttachTime (\s a -> s { _vaAttachTime = a }) . mapping _Time

-- | Indicates whether the EBS volume is deleted on instance termination.
vaDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
vaDeleteOnTermination =
    lens _vaDeleteOnTermination (\s a -> s { _vaDeleteOnTermination = a })

-- | The device name.
vaDevice :: Lens' VolumeAttachment (Maybe Text)
vaDevice = lens _vaDevice (\s a -> s { _vaDevice = a })

-- | The ID of the instance.
vaInstanceId :: Lens' VolumeAttachment (Maybe Text)
vaInstanceId = lens _vaInstanceId (\s a -> s { _vaInstanceId = a })

-- | The attachment state of the volume.
vaState :: Lens' VolumeAttachment (Maybe VolumeAttachmentState)
vaState = lens _vaState (\s a -> s { _vaState = a })

-- | The ID of the volume.
vaVolumeId :: Lens' VolumeAttachment (Maybe Text)
vaVolumeId = lens _vaVolumeId (\s a -> s { _vaVolumeId = a })

instance FromXML VolumeAttachment where
    parseXML x = VolumeAttachment
        <$> x .@? "attachTime"
        <*> x .@? "deleteOnTermination"
        <*> x .@? "device"
        <*> x .@? "instanceId"
        <*> x .@? "status"
        <*> x .@? "volumeId"

instance ToQuery VolumeAttachment where
    toQuery VolumeAttachment{..} = mconcat
        [ "AttachTime"          =? _vaAttachTime
        , "DeleteOnTermination" =? _vaDeleteOnTermination
        , "Device"              =? _vaDevice
        , "InstanceId"          =? _vaInstanceId
        , "Status"              =? _vaState
        , "VolumeId"            =? _vaVolumeId
        ]

data CustomerGateway = CustomerGateway
    { _cgBgpAsn            :: Text
    , _cgCustomerGatewayId :: Text
    , _cgIpAddress         :: Text
    , _cgState             :: Text
    , _cgTags              :: List "item" Tag
    , _cgType              :: Text
    } deriving (Eq, Read, Show)

-- | 'CustomerGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgBgpAsn' @::@ 'Text'
--
-- * 'cgCustomerGatewayId' @::@ 'Text'
--
-- * 'cgIpAddress' @::@ 'Text'
--
-- * 'cgState' @::@ 'Text'
--
-- * 'cgTags' @::@ ['Tag']
--
-- * 'cgType' @::@ 'Text'
--
customerGateway :: Text -- ^ 'cgCustomerGatewayId'
                -> Text -- ^ 'cgState'
                -> Text -- ^ 'cgType'
                -> Text -- ^ 'cgIpAddress'
                -> Text -- ^ 'cgBgpAsn'
                -> CustomerGateway
customerGateway p1 p2 p3 p4 p5 = CustomerGateway
    { _cgCustomerGatewayId = p1
    , _cgState             = p2
    , _cgType              = p3
    , _cgIpAddress         = p4
    , _cgBgpAsn            = p5
    , _cgTags              = mempty
    }

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number
-- (ASN).
cgBgpAsn :: Lens' CustomerGateway Text
cgBgpAsn = lens _cgBgpAsn (\s a -> s { _cgBgpAsn = a })

-- | The ID of the customer gateway.
cgCustomerGatewayId :: Lens' CustomerGateway Text
cgCustomerGatewayId =
    lens _cgCustomerGatewayId (\s a -> s { _cgCustomerGatewayId = a })

-- | The Internet-routable IP address of the customer gateway's outside interface.
cgIpAddress :: Lens' CustomerGateway Text
cgIpAddress = lens _cgIpAddress (\s a -> s { _cgIpAddress = a })

-- | The current state of the customer gateway ('pending | available | deleting |deleted').
cgState :: Lens' CustomerGateway Text
cgState = lens _cgState (\s a -> s { _cgState = a })

-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway [Tag]
cgTags = lens _cgTags (\s a -> s { _cgTags = a }) . _List

-- | The type of VPN connection the customer gateway supports ('ipsec.1').
cgType :: Lens' CustomerGateway Text
cgType = lens _cgType (\s a -> s { _cgType = a })

instance FromXML CustomerGateway where
    parseXML x = CustomerGateway
        <$> x .@  "bgpAsn"
        <*> x .@  "customerGatewayId"
        <*> x .@  "ipAddress"
        <*> x .@  "state"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "type"

instance ToQuery CustomerGateway where
    toQuery CustomerGateway{..} = mconcat
        [ "BgpAsn"            =? _cgBgpAsn
        , "CustomerGatewayId" =? _cgCustomerGatewayId
        , "IpAddress"         =? _cgIpAddress
        , "State"             =? _cgState
        , "TagSet"            `toQueryList` _cgTags
        , "Type"              =? _cgType
        ]

data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdAttachTime          :: Maybe ISO8601
    , _eibdDeleteOnTermination :: Maybe Bool
    , _eibdStatus              :: Maybe AttachmentStatus
    , _eibdVolumeId            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'EbsInstanceBlockDevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eibdAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'eibdDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'eibdStatus' @::@ 'Maybe' 'AttachmentStatus'
--
-- * 'eibdVolumeId' @::@ 'Maybe' 'Text'
--
ebsInstanceBlockDevice :: EbsInstanceBlockDevice
ebsInstanceBlockDevice = EbsInstanceBlockDevice
    { _eibdVolumeId            = Nothing
    , _eibdStatus              = Nothing
    , _eibdAttachTime          = Nothing
    , _eibdDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EbsInstanceBlockDevice (Maybe UTCTime)
eibdAttachTime = lens _eibdAttachTime (\s a -> s { _eibdAttachTime = a }) . mapping _Time

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EbsInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination =
    lens _eibdDeleteOnTermination (\s a -> s { _eibdDeleteOnTermination = a })

-- | The attachment state.
eibdStatus :: Lens' EbsInstanceBlockDevice (Maybe AttachmentStatus)
eibdStatus = lens _eibdStatus (\s a -> s { _eibdStatus = a })

-- | The ID of the EBS volume.
eibdVolumeId :: Lens' EbsInstanceBlockDevice (Maybe Text)
eibdVolumeId = lens _eibdVolumeId (\s a -> s { _eibdVolumeId = a })

instance FromXML EbsInstanceBlockDevice where
    parseXML x = EbsInstanceBlockDevice
        <$> x .@? "attachTime"
        <*> x .@? "deleteOnTermination"
        <*> x .@? "status"
        <*> x .@? "volumeId"

instance ToQuery EbsInstanceBlockDevice where
    toQuery EbsInstanceBlockDevice{..} = mconcat
        [ "AttachTime"          =? _eibdAttachTime
        , "DeleteOnTermination" =? _eibdDeleteOnTermination
        , "Status"              =? _eibdStatus
        , "VolumeId"            =? _eibdVolumeId
        ]

data ShutdownBehavior
    = Stop      -- ^ stop
    | Terminate -- ^ terminate
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ShutdownBehavior

instance FromText ShutdownBehavior where
    parser = takeLowerText >>= \case
        "stop"      -> pure Stop
        "terminate" -> pure Terminate
        e           -> fail $
            "Failure parsing ShutdownBehavior from " ++ show e

instance ToText ShutdownBehavior where
    toText = \case
        Stop      -> "stop"
        Terminate -> "terminate"

instance ToByteString ShutdownBehavior
instance ToHeader     ShutdownBehavior
instance ToQuery      ShutdownBehavior

instance FromXML ShutdownBehavior where
    parseXML = parseXMLText "ShutdownBehavior"

data DiskImageDescription = DiskImageDescription
    { _did1Checksum          :: Maybe Text
    , _did1Format            :: DiskImageFormat
    , _did1ImportManifestUrl :: Text
    , _did1Size              :: Integer
    } deriving (Eq, Read, Show)

-- | 'DiskImageDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'did1Checksum' @::@ 'Maybe' 'Text'
--
-- * 'did1Format' @::@ 'DiskImageFormat'
--
-- * 'did1ImportManifestUrl' @::@ 'Text'
--
-- * 'did1Size' @::@ 'Integer'
--
diskImageDescription :: DiskImageFormat -- ^ 'did1Format'
                     -> Integer -- ^ 'did1Size'
                     -> Text -- ^ 'did1ImportManifestUrl'
                     -> DiskImageDescription
diskImageDescription p1 p2 p3 = DiskImageDescription
    { _did1Format            = p1
    , _did1Size              = p2
    , _did1ImportManifestUrl = p3
    , _did1Checksum          = Nothing
    }

-- | The checksum computed for the disk image.
did1Checksum :: Lens' DiskImageDescription (Maybe Text)
did1Checksum = lens _did1Checksum (\s a -> s { _did1Checksum = a })

-- | The disk image format.
did1Format :: Lens' DiskImageDescription DiskImageFormat
did1Format = lens _did1Format (\s a -> s { _did1Format = a })

-- | A presigned URL for the import manifest stored in Amazon S3. For information
-- about creating a presigned URL for an Amazon S3 object, read the "Query
-- String Request Authentication Alternative" section of the <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating RESTRequests> topic in the /Amazon Simple Storage Service Developer Guide/.
did1ImportManifestUrl :: Lens' DiskImageDescription Text
did1ImportManifestUrl =
    lens _did1ImportManifestUrl (\s a -> s { _did1ImportManifestUrl = a })

-- | The size of the disk image, in GiB.
did1Size :: Lens' DiskImageDescription Integer
did1Size = lens _did1Size (\s a -> s { _did1Size = a })

instance FromXML DiskImageDescription where
    parseXML x = DiskImageDescription
        <$> x .@? "checksum"
        <*> x .@  "format"
        <*> x .@  "importManifestUrl"
        <*> x .@  "size"

instance ToQuery DiskImageDescription where
    toQuery DiskImageDescription{..} = mconcat
        [ "Checksum"          =? _did1Checksum
        , "Format"            =? _did1Format
        , "ImportManifestUrl" =? _did1ImportManifestUrl
        , "Size"              =? _did1Size
        ]

data DiskImageVolumeDescription = DiskImageVolumeDescription
    { _divdId   :: Text
    , _divdSize :: Maybe Integer
    } deriving (Eq, Ord, Read, Show)

-- | 'DiskImageVolumeDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'divdId' @::@ 'Text'
--
-- * 'divdSize' @::@ 'Maybe' 'Integer'
--
diskImageVolumeDescription :: Text -- ^ 'divdId'
                           -> DiskImageVolumeDescription
diskImageVolumeDescription p1 = DiskImageVolumeDescription
    { _divdId   = p1
    , _divdSize = Nothing
    }

-- | The volume identifier.
divdId :: Lens' DiskImageVolumeDescription Text
divdId = lens _divdId (\s a -> s { _divdId = a })

-- | The size of the volume, in GiB.
divdSize :: Lens' DiskImageVolumeDescription (Maybe Integer)
divdSize = lens _divdSize (\s a -> s { _divdSize = a })

instance FromXML DiskImageVolumeDescription where
    parseXML x = DiskImageVolumeDescription
        <$> x .@  "id"
        <*> x .@? "size"

instance ToQuery DiskImageVolumeDescription where
    toQuery DiskImageVolumeDescription{..} = mconcat
        [ "Id"   =? _divdId
        , "Size" =? _divdSize
        ]

newtype Monitoring = Monitoring
    { _mState :: Maybe MonitoringState
    } deriving (Eq, Read, Show)

-- | 'Monitoring' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mState' @::@ 'Maybe' 'MonitoringState'
--
monitoring :: Monitoring
monitoring = Monitoring
    { _mState = Nothing
    }

-- | Indicates whether monitoring is enabled for the instance.
mState :: Lens' Monitoring (Maybe MonitoringState)
mState = lens _mState (\s a -> s { _mState = a })

instance FromXML Monitoring where
    parseXML x = Monitoring
        <$> x .@? "state"

instance ToQuery Monitoring where
    toQuery Monitoring{..} = mconcat
        [ "State" =? _mState
        ]

data SubnetState
    = SubnetStateAvailable -- ^ available
    | SubnetStatePending   -- ^ pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SubnetState

instance FromText SubnetState where
    parser = takeLowerText >>= \case
        "available" -> pure SubnetStateAvailable
        "pending"   -> pure SubnetStatePending
        e           -> fail $
            "Failure parsing SubnetState from " ++ show e

instance ToText SubnetState where
    toText = \case
        SubnetStateAvailable -> "available"
        SubnetStatePending   -> "pending"

instance ToByteString SubnetState
instance ToHeader     SubnetState
instance ToQuery      SubnetState

instance FromXML SubnetState where
    parseXML = parseXMLText "SubnetState"

data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem
    { _csfrsiCurrentSpotFleetRequestState  :: BatchState
    , _csfrsiPreviousSpotFleetRequestState :: BatchState
    , _csfrsiSpotFleetRequestId            :: Text
    } deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsSuccessItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfrsiCurrentSpotFleetRequestState' @::@ 'BatchState'
--
-- * 'csfrsiPreviousSpotFleetRequestState' @::@ 'BatchState'
--
-- * 'csfrsiSpotFleetRequestId' @::@ 'Text'
--
cancelSpotFleetRequestsSuccessItem :: Text -- ^ 'csfrsiSpotFleetRequestId'
                                   -> BatchState -- ^ 'csfrsiCurrentSpotFleetRequestState'
                                   -> BatchState -- ^ 'csfrsiPreviousSpotFleetRequestState'
                                   -> CancelSpotFleetRequestsSuccessItem
cancelSpotFleetRequestsSuccessItem p1 p2 p3 = CancelSpotFleetRequestsSuccessItem
    { _csfrsiSpotFleetRequestId            = p1
    , _csfrsiCurrentSpotFleetRequestState  = p2
    , _csfrsiPreviousSpotFleetRequestState = p3
    }

-- | The current state of the Spot fleet request.
csfrsiCurrentSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem BatchState
csfrsiCurrentSpotFleetRequestState =
    lens _csfrsiCurrentSpotFleetRequestState
        (\s a -> s { _csfrsiCurrentSpotFleetRequestState = a })

-- | The previous state of the Spot fleet request.
csfrsiPreviousSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem BatchState
csfrsiPreviousSpotFleetRequestState =
    lens _csfrsiPreviousSpotFleetRequestState
        (\s a -> s { _csfrsiPreviousSpotFleetRequestState = a })

-- | The ID of the Spot fleet request.
csfrsiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsSuccessItem Text
csfrsiSpotFleetRequestId =
    lens _csfrsiSpotFleetRequestId
        (\s a -> s { _csfrsiSpotFleetRequestId = a })

instance FromXML CancelSpotFleetRequestsSuccessItem where
    parseXML x = CancelSpotFleetRequestsSuccessItem
        <$> x .@  "currentSpotFleetRequestState"
        <*> x .@  "previousSpotFleetRequestState"
        <*> x .@  "spotFleetRequestId"

instance ToQuery CancelSpotFleetRequestsSuccessItem where
    toQuery CancelSpotFleetRequestsSuccessItem{..} = mconcat
        [ "CurrentSpotFleetRequestState"  =? _csfrsiCurrentSpotFleetRequestState
        , "PreviousSpotFleetRequestState" =? _csfrsiPreviousSpotFleetRequestState
        , "SpotFleetRequestId"            =? _csfrsiSpotFleetRequestId
        ]

data ContainerFormat
    = Ova -- ^ ova
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ContainerFormat

instance FromText ContainerFormat where
    parser = takeLowerText >>= \case
        "ova" -> pure Ova
        e     -> fail $
            "Failure parsing ContainerFormat from " ++ show e

instance ToText ContainerFormat where
    toText Ova = "ova"

instance ToByteString ContainerFormat
instance ToHeader     ContainerFormat
instance ToQuery      ContainerFormat

instance FromXML ContainerFormat where
    parseXML = parseXMLText "ContainerFormat"

newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AvailabilityZoneMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azmMessage' @::@ 'Maybe' 'Text'
--
availabilityZoneMessage :: AvailabilityZoneMessage
availabilityZoneMessage = AvailabilityZoneMessage
    { _azmMessage = Nothing
    }

-- | The message about the Availability Zone.
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage = lens _azmMessage (\s a -> s { _azmMessage = a })

instance FromXML AvailabilityZoneMessage where
    parseXML x = AvailabilityZoneMessage
        <$> x .@? "message"

instance ToQuery AvailabilityZoneMessage where
    toQuery AvailabilityZoneMessage{..} = mconcat
        [ "Message" =? _azmMessage
        ]

data VpcAttachment = VpcAttachment
    { _va1State :: Maybe AttachmentStatus
    , _va1VpcId :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VpcAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'va1State' @::@ 'Maybe' 'AttachmentStatus'
--
-- * 'va1VpcId' @::@ 'Maybe' 'Text'
--
vpcAttachment :: VpcAttachment
vpcAttachment = VpcAttachment
    { _va1VpcId = Nothing
    , _va1State = Nothing
    }

-- | The current state of the attachment.
va1State :: Lens' VpcAttachment (Maybe AttachmentStatus)
va1State = lens _va1State (\s a -> s { _va1State = a })

-- | The ID of the VPC.
va1VpcId :: Lens' VpcAttachment (Maybe Text)
va1VpcId = lens _va1VpcId (\s a -> s { _va1VpcId = a })

instance FromXML VpcAttachment where
    parseXML x = VpcAttachment
        <$> x .@? "state"
        <*> x .@? "vpcId"

instance ToQuery VpcAttachment where
    toQuery VpcAttachment{..} = mconcat
        [ "State" =? _va1State
        , "VpcId" =? _va1VpcId
        ]

data EventType
    = Error              -- ^ error
    | FleetRequestChange -- ^ fleetRequestChange
    | InstanceChange     -- ^ instanceChange
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable EventType

instance FromText EventType where
    parser = takeLowerText >>= \case
        "error"              -> pure Error
        "fleetrequestchange" -> pure FleetRequestChange
        "instancechange"     -> pure InstanceChange
        e                    -> fail $
            "Failure parsing EventType from " ++ show e

instance ToText EventType where
    toText = \case
        Error              -> "error"
        FleetRequestChange -> "fleetRequestChange"
        InstanceChange     -> "instanceChange"

instance ToByteString EventType
instance ToHeader     EventType
instance ToQuery      EventType

instance FromXML EventType where
    parseXML = parseXMLText "EventType"

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName :: Maybe Text
    , _ibdmEbs        :: Maybe EbsInstanceBlockDevice
    } deriving (Eq, Read, Show)

-- | 'InstanceBlockDeviceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibdmDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'ibdmEbs' @::@ 'Maybe' 'EbsInstanceBlockDevice'
--
instanceBlockDeviceMapping :: InstanceBlockDeviceMapping
instanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { _ibdmDeviceName = Nothing
    , _ibdmEbs        = Nothing
    }

-- | The device name exposed to the instance (for example, '/dev/sdh' or 'xvdh').
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName = lens _ibdmDeviceName (\s a -> s { _ibdmDeviceName = a })

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
ibdmEbs :: Lens' InstanceBlockDeviceMapping (Maybe EbsInstanceBlockDevice)
ibdmEbs = lens _ibdmEbs (\s a -> s { _ibdmEbs = a })

instance FromXML InstanceBlockDeviceMapping where
    parseXML x = InstanceBlockDeviceMapping
        <$> x .@? "deviceName"
        <*> x .@? "ebs"

instance ToQuery InstanceBlockDeviceMapping where
    toQuery InstanceBlockDeviceMapping{..} = mconcat
        [ "DeviceName" =? _ibdmDeviceName
        , "Ebs"        =? _ibdmEbs
        ]

data StatusType
    = STFailed           -- ^ failed
    | STInsufficientData -- ^ insufficient-data
    | STPassed           -- ^ passed
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable StatusType

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "failed"            -> pure STFailed
        "insufficient-data" -> pure STInsufficientData
        "passed"            -> pure STPassed
        e                   -> fail $
            "Failure parsing StatusType from " ++ show e

instance ToText StatusType where
    toText = \case
        STFailed           -> "failed"
        STInsufficientData -> "insufficient-data"
        STPassed           -> "passed"

instance ToByteString StatusType
instance ToHeader     StatusType
instance ToQuery      StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsContainerFormat :: Maybe ContainerFormat
    , _etstsDiskImageFormat :: Maybe DiskImageFormat
    , _etstsS3Bucket        :: Maybe Text
    , _etstsS3Prefix        :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ExportToS3TaskSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etstsContainerFormat' @::@ 'Maybe' 'ContainerFormat'
--
-- * 'etstsDiskImageFormat' @::@ 'Maybe' 'DiskImageFormat'
--
-- * 'etstsS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'etstsS3Prefix' @::@ 'Maybe' 'Text'
--
exportToS3TaskSpecification :: ExportToS3TaskSpecification
exportToS3TaskSpecification = ExportToS3TaskSpecification
    { _etstsDiskImageFormat = Nothing
    , _etstsContainerFormat = Nothing
    , _etstsS3Bucket        = Nothing
    , _etstsS3Prefix        = Nothing
    }

-- | The container format used to combine disk images with metadata (such as OVF).
-- If absent, only the disk image is exported.
etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe ContainerFormat)
etstsContainerFormat =
    lens _etstsContainerFormat (\s a -> s { _etstsContainerFormat = a })

-- | The format for the exported image.
etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe DiskImageFormat)
etstsDiskImageFormat =
    lens _etstsDiskImageFormat (\s a -> s { _etstsDiskImageFormat = a })

-- | The S3 bucket for the destination image. The destination bucket must exist
-- and grant WRITE and READ_ACP permissions to the AWS account 'vm-import-export@amazon.com'.
etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket = lens _etstsS3Bucket (\s a -> s { _etstsS3Bucket = a })

-- | The image is written to a single object in the S3 bucket at the S3 key
-- s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix = lens _etstsS3Prefix (\s a -> s { _etstsS3Prefix = a })

instance FromXML ExportToS3TaskSpecification where
    parseXML x = ExportToS3TaskSpecification
        <$> x .@? "containerFormat"
        <*> x .@? "diskImageFormat"
        <*> x .@? "s3Bucket"
        <*> x .@? "s3Prefix"

instance ToQuery ExportToS3TaskSpecification where
    toQuery ExportToS3TaskSpecification{..} = mconcat
        [ "ContainerFormat" =? _etstsContainerFormat
        , "DiskImageFormat" =? _etstsDiskImageFormat
        , "S3Bucket"        =? _etstsS3Bucket
        , "S3Prefix"        =? _etstsS3Prefix
        ]

data CancelBatchErrorCode
    = FleetRequestIdDoesNotExist        -- ^ fleetRequestIdDoesNotExist
    | FleetRequestIdMalformed           -- ^ fleetRequestIdMalformed
    | FleetRequestNotInCancellableState -- ^ fleetRequestNotInCancellableState
    | UnexpectedError                   -- ^ unexpectedError
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable CancelBatchErrorCode

instance FromText CancelBatchErrorCode where
    parser = takeLowerText >>= \case
        "fleetrequestiddoesnotexist"        -> pure FleetRequestIdDoesNotExist
        "fleetrequestidmalformed"           -> pure FleetRequestIdMalformed
        "fleetrequestnotincancellablestate" -> pure FleetRequestNotInCancellableState
        "unexpectederror"                   -> pure UnexpectedError
        e                                   -> fail $
            "Failure parsing CancelBatchErrorCode from " ++ show e

instance ToText CancelBatchErrorCode where
    toText = \case
        FleetRequestIdDoesNotExist        -> "fleetRequestIdDoesNotExist"
        FleetRequestIdMalformed           -> "fleetRequestIdMalformed"
        FleetRequestNotInCancellableState -> "fleetRequestNotInCancellableState"
        UnexpectedError                   -> "unexpectedError"

instance ToByteString CancelBatchErrorCode
instance ToHeader     CancelBatchErrorCode
instance ToQuery      CancelBatchErrorCode

instance FromXML CancelBatchErrorCode where
    parseXML = parseXMLText "CancelBatchErrorCode"

newtype PrefixListId = PrefixListId
    { _pliPrefixListId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'PrefixListId' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pliPrefixListId' @::@ 'Maybe' 'Text'
--
prefixListId :: PrefixListId
prefixListId = PrefixListId
    { _pliPrefixListId = Nothing
    }

-- | The ID of the prefix.
pliPrefixListId :: Lens' PrefixListId (Maybe Text)
pliPrefixListId = lens _pliPrefixListId (\s a -> s { _pliPrefixListId = a })

instance FromXML PrefixListId where
    parseXML x = PrefixListId
        <$> x .@? "prefixListId"

instance ToQuery PrefixListId where
    toQuery PrefixListId{..} = mconcat
        [ "PrefixListId" =? _pliPrefixListId
        ]

data NetworkInterfaceAttribute
    = Attachment      -- ^ attachment
    | Description     -- ^ description
    | GroupSet        -- ^ groupSet
    | SourceDestCheck -- ^ sourceDestCheck
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable NetworkInterfaceAttribute

instance FromText NetworkInterfaceAttribute where
    parser = takeLowerText >>= \case
        "attachment"      -> pure Attachment
        "description"     -> pure Description
        "groupset"        -> pure GroupSet
        "sourcedestcheck" -> pure SourceDestCheck
        e                 -> fail $
            "Failure parsing NetworkInterfaceAttribute from " ++ show e

instance ToText NetworkInterfaceAttribute where
    toText = \case
        Attachment      -> "attachment"
        Description     -> "description"
        GroupSet        -> "groupSet"
        SourceDestCheck -> "sourceDestCheck"

instance ToByteString NetworkInterfaceAttribute
instance ToHeader     NetworkInterfaceAttribute
instance ToQuery      NetworkInterfaceAttribute

instance FromXML NetworkInterfaceAttribute where
    parseXML = parseXMLText "NetworkInterfaceAttribute"

data ImageTypeValues
    = Kernel  -- ^ kernel
    | Machine -- ^ machine
    | Ramdisk -- ^ ramdisk
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ImageTypeValues

instance FromText ImageTypeValues where
    parser = takeLowerText >>= \case
        "kernel"  -> pure Kernel
        "machine" -> pure Machine
        "ramdisk" -> pure Ramdisk
        e         -> fail $
            "Failure parsing ImageTypeValues from " ++ show e

instance ToText ImageTypeValues where
    toText = \case
        Kernel  -> "kernel"
        Machine -> "machine"
        Ramdisk -> "ramdisk"

instance ToByteString ImageTypeValues
instance ToHeader     ImageTypeValues
instance ToQuery      ImageTypeValues

instance FromXML ImageTypeValues where
    parseXML = parseXMLText "ImageTypeValues"

data InstanceExportDetails = InstanceExportDetails
    { _iedInstanceId        :: Maybe Text
    , _iedTargetEnvironment :: Maybe ExportEnvironment
    } deriving (Eq, Read, Show)

-- | 'InstanceExportDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iedInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iedTargetEnvironment' @::@ 'Maybe' 'ExportEnvironment'
--
instanceExportDetails :: InstanceExportDetails
instanceExportDetails = InstanceExportDetails
    { _iedInstanceId        = Nothing
    , _iedTargetEnvironment = Nothing
    }

-- | The ID of the resource being exported.
iedInstanceId :: Lens' InstanceExportDetails (Maybe Text)
iedInstanceId = lens _iedInstanceId (\s a -> s { _iedInstanceId = a })

-- | The target virtualization environment.
iedTargetEnvironment :: Lens' InstanceExportDetails (Maybe ExportEnvironment)
iedTargetEnvironment =
    lens _iedTargetEnvironment (\s a -> s { _iedTargetEnvironment = a })

instance FromXML InstanceExportDetails where
    parseXML x = InstanceExportDetails
        <$> x .@? "instanceId"
        <*> x .@? "targetEnvironment"

instance ToQuery InstanceExportDetails where
    toQuery InstanceExportDetails{..} = mconcat
        [ "InstanceId"        =? _iedInstanceId
        , "TargetEnvironment" =? _iedTargetEnvironment
        ]

data SnapshotAttributeName
    = SANCreateVolumePermission -- ^ createVolumePermission
    | SANProductCodes           -- ^ productCodes
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SnapshotAttributeName

instance FromText SnapshotAttributeName where
    parser = takeLowerText >>= \case
        "createvolumepermission" -> pure SANCreateVolumePermission
        "productcodes"           -> pure SANProductCodes
        e                        -> fail $
            "Failure parsing SnapshotAttributeName from " ++ show e

instance ToText SnapshotAttributeName where
    toText = \case
        SANCreateVolumePermission -> "createVolumePermission"
        SANProductCodes           -> "productCodes"

instance ToByteString SnapshotAttributeName
instance ToHeader     SnapshotAttributeName
instance ToQuery      SnapshotAttributeName

instance FromXML SnapshotAttributeName where
    parseXML = parseXMLText "SnapshotAttributeName"

data AvailabilityZone = AvailabilityZone
    { _azMessages   :: List "item" AvailabilityZoneMessage
    , _azRegionName :: Maybe Text
    , _azState      :: Maybe AvailabilityZoneState
    , _azZoneName   :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'AvailabilityZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azMessages' @::@ ['AvailabilityZoneMessage']
--
-- * 'azRegionName' @::@ 'Maybe' 'Text'
--
-- * 'azState' @::@ 'Maybe' 'AvailabilityZoneState'
--
-- * 'azZoneName' @::@ 'Maybe' 'Text'
--
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone
    { _azZoneName   = Nothing
    , _azState      = Nothing
    , _azRegionName = Nothing
    , _azMessages   = mempty
    }

-- | Any messages about the Availability Zone.
azMessages :: Lens' AvailabilityZone [AvailabilityZoneMessage]
azMessages = lens _azMessages (\s a -> s { _azMessages = a }) . _List

-- | The name of the region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName = lens _azRegionName (\s a -> s { _azRegionName = a })

-- | The state of the Availability Zone ('available' | 'impaired' | 'unavailable').
azState :: Lens' AvailabilityZone (Maybe AvailabilityZoneState)
azState = lens _azState (\s a -> s { _azState = a })

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\s a -> s { _azZoneName = a })

instance FromXML AvailabilityZone where
    parseXML x = AvailabilityZone
        <$> x .@? "messageSet" .!@ mempty
        <*> x .@? "regionName"
        <*> x .@? "zoneState"
        <*> x .@? "zoneName"

instance ToQuery AvailabilityZone where
    toQuery AvailabilityZone{..} = mconcat
        [ "MessageSet" `toQueryList` _azMessages
        , "RegionName" =? _azRegionName
        , "ZoneState"  =? _azState
        , "ZoneName"   =? _azZoneName
        ]

data HistoryRecord = HistoryRecord
    { _hrEventInformation :: EventInformation
    , _hrEventType        :: EventType
    , _hrTimestamp        :: ISO8601
    } deriving (Eq, Read, Show)

-- | 'HistoryRecord' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hrEventInformation' @::@ 'EventInformation'
--
-- * 'hrEventType' @::@ 'EventType'
--
-- * 'hrTimestamp' @::@ 'UTCTime'
--
historyRecord :: UTCTime -- ^ 'hrTimestamp'
              -> EventType -- ^ 'hrEventType'
              -> EventInformation -- ^ 'hrEventInformation'
              -> HistoryRecord
historyRecord p1 p2 p3 = HistoryRecord
    { _hrTimestamp        = withIso _Time (const id) p1
    , _hrEventType        = p2
    , _hrEventInformation = p3
    }

-- | Information about the event.
hrEventInformation :: Lens' HistoryRecord EventInformation
hrEventInformation =
    lens _hrEventInformation (\s a -> s { _hrEventInformation = a })

-- | The event type.
--
-- 'error' - Indicates an error with the Spot fleet request.
--
-- 'fleetRequestChange' - Indicates a change in the status or configuration of
-- the Spot fleet request.
--
-- 'instanceChange' - Indicates that an instance was launched or terminated.
--
--
hrEventType :: Lens' HistoryRecord EventType
hrEventType = lens _hrEventType (\s a -> s { _hrEventType = a })

-- | The date and time of the event, in UTC format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
hrTimestamp :: Lens' HistoryRecord UTCTime
hrTimestamp = lens _hrTimestamp (\s a -> s { _hrTimestamp = a }) . _Time

instance FromXML HistoryRecord where
    parseXML x = HistoryRecord
        <$> x .@  "eventInformation"
        <*> x .@  "eventType"
        <*> x .@  "timestamp"

instance ToQuery HistoryRecord where
    toQuery HistoryRecord{..} = mconcat
        [ "EventInformation" =? _hrEventInformation
        , "EventType"        =? _hrEventType
        , "Timestamp"        =? _hrTimestamp
        ]

data ImportImageTask = ImportImageTask
    { _iitArchitecture    :: Maybe Text
    , _iitDescription     :: Maybe Text
    , _iitHypervisor      :: Maybe Text
    , _iitImageId         :: Maybe Text
    , _iitImportTaskId    :: Maybe Text
    , _iitLicenseType     :: Maybe Text
    , _iitPlatform        :: Maybe Text
    , _iitProgress        :: Maybe Text
    , _iitSnapshotDetails :: List "item" SnapshotDetail
    , _iitStatus          :: Maybe Text
    , _iitStatusMessage   :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ImportImageTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iitArchitecture' @::@ 'Maybe' 'Text'
--
-- * 'iitDescription' @::@ 'Maybe' 'Text'
--
-- * 'iitHypervisor' @::@ 'Maybe' 'Text'
--
-- * 'iitImageId' @::@ 'Maybe' 'Text'
--
-- * 'iitImportTaskId' @::@ 'Maybe' 'Text'
--
-- * 'iitLicenseType' @::@ 'Maybe' 'Text'
--
-- * 'iitPlatform' @::@ 'Maybe' 'Text'
--
-- * 'iitProgress' @::@ 'Maybe' 'Text'
--
-- * 'iitSnapshotDetails' @::@ ['SnapshotDetail']
--
-- * 'iitStatus' @::@ 'Maybe' 'Text'
--
-- * 'iitStatusMessage' @::@ 'Maybe' 'Text'
--
importImageTask :: ImportImageTask
importImageTask = ImportImageTask
    { _iitImportTaskId    = Nothing
    , _iitArchitecture    = Nothing
    , _iitLicenseType     = Nothing
    , _iitPlatform        = Nothing
    , _iitHypervisor      = Nothing
    , _iitDescription     = Nothing
    , _iitSnapshotDetails = mempty
    , _iitImageId         = Nothing
    , _iitProgress        = Nothing
    , _iitStatusMessage   = Nothing
    , _iitStatus          = Nothing
    }

-- | The architecture of the virtual machine.
--
-- Valid values: 'i386' | 'x86_64'
iitArchitecture :: Lens' ImportImageTask (Maybe Text)
iitArchitecture = lens _iitArchitecture (\s a -> s { _iitArchitecture = a })

-- | A description of the import task.
iitDescription :: Lens' ImportImageTask (Maybe Text)
iitDescription = lens _iitDescription (\s a -> s { _iitDescription = a })

-- | The target hypervisor for the import task.
--
-- Valid values: 'xen'
iitHypervisor :: Lens' ImportImageTask (Maybe Text)
iitHypervisor = lens _iitHypervisor (\s a -> s { _iitHypervisor = a })

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
iitImageId :: Lens' ImportImageTask (Maybe Text)
iitImageId = lens _iitImageId (\s a -> s { _iitImageId = a })

-- | The ID of the import image task.
iitImportTaskId :: Lens' ImportImageTask (Maybe Text)
iitImportTaskId = lens _iitImportTaskId (\s a -> s { _iitImportTaskId = a })

-- | The license type of the virtual machine.
iitLicenseType :: Lens' ImportImageTask (Maybe Text)
iitLicenseType = lens _iitLicenseType (\s a -> s { _iitLicenseType = a })

-- | The description string for the import image task.
iitPlatform :: Lens' ImportImageTask (Maybe Text)
iitPlatform = lens _iitPlatform (\s a -> s { _iitPlatform = a })

-- | The percentage of progress of the import image task.
iitProgress :: Lens' ImportImageTask (Maybe Text)
iitProgress = lens _iitProgress (\s a -> s { _iitProgress = a })

-- | Information about the snapshots.
iitSnapshotDetails :: Lens' ImportImageTask [SnapshotDetail]
iitSnapshotDetails =
    lens _iitSnapshotDetails (\s a -> s { _iitSnapshotDetails = a })
        . _List

-- | A brief status for the import image task.
iitStatus :: Lens' ImportImageTask (Maybe Text)
iitStatus = lens _iitStatus (\s a -> s { _iitStatus = a })

-- | A descriptive status message for the import image task.
iitStatusMessage :: Lens' ImportImageTask (Maybe Text)
iitStatusMessage = lens _iitStatusMessage (\s a -> s { _iitStatusMessage = a })

instance FromXML ImportImageTask where
    parseXML x = ImportImageTask
        <$> x .@? "architecture"
        <*> x .@? "description"
        <*> x .@? "hypervisor"
        <*> x .@? "imageId"
        <*> x .@? "importTaskId"
        <*> x .@? "licenseType"
        <*> x .@? "platform"
        <*> x .@? "progress"
        <*> x .@? "snapshotDetailSet" .!@ mempty
        <*> x .@? "status"
        <*> x .@? "statusMessage"

instance ToQuery ImportImageTask where
    toQuery ImportImageTask{..} = mconcat
        [ "Architecture"      =? _iitArchitecture
        , "Description"       =? _iitDescription
        , "Hypervisor"        =? _iitHypervisor
        , "ImageId"           =? _iitImageId
        , "ImportTaskId"      =? _iitImportTaskId
        , "LicenseType"       =? _iitLicenseType
        , "Platform"          =? _iitPlatform
        , "Progress"          =? _iitProgress
        , "SnapshotDetailSet" `toQueryList` _iitSnapshotDetails
        , "Status"            =? _iitStatus
        , "StatusMessage"     =? _iitStatusMessage
        ]

data VpnState
    = VpnStateAvailable -- ^ available
    | VpnStateDeleted   -- ^ deleted
    | VpnStateDeleting  -- ^ deleting
    | VpnStatePending   -- ^ pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VpnState

instance FromText VpnState where
    parser = takeLowerText >>= \case
        "available" -> pure VpnStateAvailable
        "deleted"   -> pure VpnStateDeleted
        "deleting"  -> pure VpnStateDeleting
        "pending"   -> pure VpnStatePending
        e           -> fail $
            "Failure parsing VpnState from " ++ show e

instance ToText VpnState where
    toText = \case
        VpnStateAvailable -> "available"
        VpnStateDeleted   -> "deleted"
        VpnStateDeleting  -> "deleting"
        VpnStatePending   -> "pending"

instance ToByteString VpnState
instance ToHeader     VpnState
instance ToQuery      VpnState

instance FromXML VpnState where
    parseXML = parseXMLText "VpnState"

data RouteTable = RouteTable
    { _rtAssociations    :: List "item" RouteTableAssociation
    , _rtPropagatingVgws :: List "item" PropagatingVgw
    , _rtRouteTableId    :: Maybe Text
    , _rtRoutes          :: List "item" Route
    , _rtTags            :: List "item" Tag
    , _rtVpcId           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'RouteTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtAssociations' @::@ ['RouteTableAssociation']
--
-- * 'rtPropagatingVgws' @::@ ['PropagatingVgw']
--
-- * 'rtRouteTableId' @::@ 'Maybe' 'Text'
--
-- * 'rtRoutes' @::@ ['Route']
--
-- * 'rtTags' @::@ ['Tag']
--
-- * 'rtVpcId' @::@ 'Maybe' 'Text'
--
routeTable :: RouteTable
routeTable = RouteTable
    { _rtRouteTableId    = Nothing
    , _rtVpcId           = Nothing
    , _rtRoutes          = mempty
    , _rtAssociations    = mempty
    , _rtTags            = mempty
    , _rtPropagatingVgws = mempty
    }

-- | The associations between the route table and one or more subnets.
rtAssociations :: Lens' RouteTable [RouteTableAssociation]
rtAssociations = lens _rtAssociations (\s a -> s { _rtAssociations = a }) . _List

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVgws :: Lens' RouteTable [PropagatingVgw]
rtPropagatingVgws =
    lens _rtPropagatingVgws (\s a -> s { _rtPropagatingVgws = a })
        . _List

-- | The ID of the route table.
rtRouteTableId :: Lens' RouteTable (Maybe Text)
rtRouteTableId = lens _rtRouteTableId (\s a -> s { _rtRouteTableId = a })

-- | The routes in the route table.
rtRoutes :: Lens' RouteTable [Route]
rtRoutes = lens _rtRoutes (\s a -> s { _rtRoutes = a }) . _List

-- | Any tags assigned to the route table.
rtTags :: Lens' RouteTable [Tag]
rtTags = lens _rtTags (\s a -> s { _rtTags = a }) . _List

-- | The ID of the VPC.
rtVpcId :: Lens' RouteTable (Maybe Text)
rtVpcId = lens _rtVpcId (\s a -> s { _rtVpcId = a })

instance FromXML RouteTable where
    parseXML x = RouteTable
        <$> x .@? "associationSet" .!@ mempty
        <*> x .@? "propagatingVgwSet" .!@ mempty
        <*> x .@? "routeTableId"
        <*> x .@? "routeSet" .!@ mempty
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "vpcId"

instance ToQuery RouteTable where
    toQuery RouteTable{..} = mconcat
        [ "AssociationSet"    `toQueryList` _rtAssociations
        , "PropagatingVgwSet" `toQueryList` _rtPropagatingVgws
        , "RouteTableId"      =? _rtRouteTableId
        , "RouteSet"          `toQueryList` _rtRoutes
        , "TagSet"            `toQueryList` _rtTags
        , "VpcId"             =? _rtVpcId
        ]

data UserBucket = UserBucket
    { _ubS3Bucket :: Maybe Text
    , _ubS3Key    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UserBucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'ubS3Key' @::@ 'Maybe' 'Text'
--
userBucket :: UserBucket
userBucket = UserBucket
    { _ubS3Bucket = Nothing
    , _ubS3Key    = Nothing
    }

-- | The name of the S3 bucket where the disk image is located.
ubS3Bucket :: Lens' UserBucket (Maybe Text)
ubS3Bucket = lens _ubS3Bucket (\s a -> s { _ubS3Bucket = a })

-- | The key for the disk image.
ubS3Key :: Lens' UserBucket (Maybe Text)
ubS3Key = lens _ubS3Key (\s a -> s { _ubS3Key = a })

instance FromXML UserBucket where
    parseXML x = UserBucket
        <$> x .@? "S3Bucket"
        <*> x .@? "S3Key"

instance ToQuery UserBucket where
    toQuery UserBucket{..} = mconcat
        [ "S3Bucket" =? _ubS3Bucket
        , "S3Key"    =? _ubS3Key
        ]

data HypervisorType
    = Ovm -- ^ ovm
    | Xen -- ^ xen
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable HypervisorType

instance FromText HypervisorType where
    parser = takeLowerText >>= \case
        "ovm" -> pure Ovm
        "xen" -> pure Xen
        e     -> fail $
            "Failure parsing HypervisorType from " ++ show e

instance ToText HypervisorType where
    toText = \case
        Ovm -> "ovm"
        Xen -> "xen"

instance ToByteString HypervisorType
instance ToHeader     HypervisorType
instance ToQuery      HypervisorType

instance FromXML HypervisorType where
    parseXML = parseXMLText "HypervisorType"

data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem
    { _csfreiError              :: CancelSpotFleetRequestsError
    , _csfreiSpotFleetRequestId :: Text
    } deriving (Eq, Read, Show)

-- | 'CancelSpotFleetRequestsErrorItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfreiError' @::@ 'CancelSpotFleetRequestsError'
--
-- * 'csfreiSpotFleetRequestId' @::@ 'Text'
--
cancelSpotFleetRequestsErrorItem :: Text -- ^ 'csfreiSpotFleetRequestId'
                                 -> CancelSpotFleetRequestsError -- ^ 'csfreiError'
                                 -> CancelSpotFleetRequestsErrorItem
cancelSpotFleetRequestsErrorItem p1 p2 = CancelSpotFleetRequestsErrorItem
    { _csfreiSpotFleetRequestId = p1
    , _csfreiError              = p2
    }

-- | The error.
csfreiError :: Lens' CancelSpotFleetRequestsErrorItem CancelSpotFleetRequestsError
csfreiError = lens _csfreiError (\s a -> s { _csfreiError = a })

-- | The ID of the Spot fleet request.
csfreiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsErrorItem Text
csfreiSpotFleetRequestId =
    lens _csfreiSpotFleetRequestId
        (\s a -> s { _csfreiSpotFleetRequestId = a })

instance FromXML CancelSpotFleetRequestsErrorItem where
    parseXML x = CancelSpotFleetRequestsErrorItem
        <$> x .@  "error"
        <*> x .@  "spotFleetRequestId"

instance ToQuery CancelSpotFleetRequestsErrorItem where
    toQuery CancelSpotFleetRequestsErrorItem{..} = mconcat
        [ "Error"              =? _csfreiError
        , "SpotFleetRequestId" =? _csfreiSpotFleetRequestId
        ]

data InstanceStatusDetails = InstanceStatusDetails
    { _isdImpairedSince :: Maybe ISO8601
    , _isdName          :: Maybe StatusName
    , _isdStatus        :: Maybe StatusType
    } deriving (Eq, Read, Show)

-- | 'InstanceStatusDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isdImpairedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'isdName' @::@ 'Maybe' 'StatusName'
--
-- * 'isdStatus' @::@ 'Maybe' 'StatusType'
--
instanceStatusDetails :: InstanceStatusDetails
instanceStatusDetails = InstanceStatusDetails
    { _isdName          = Nothing
    , _isdStatus        = Nothing
    , _isdImpairedSince = Nothing
    }

-- | The time when a status check failed. For an instance that was launched and
-- impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe UTCTime)
isdImpairedSince = lens _isdImpairedSince (\s a -> s { _isdImpairedSince = a }) . mapping _Time

-- | The type of instance status.
isdName :: Lens' InstanceStatusDetails (Maybe StatusName)
isdName = lens _isdName (\s a -> s { _isdName = a })

-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe StatusType)
isdStatus = lens _isdStatus (\s a -> s { _isdStatus = a })

instance FromXML InstanceStatusDetails where
    parseXML x = InstanceStatusDetails
        <$> x .@? "impairedSince"
        <*> x .@? "name"
        <*> x .@? "status"

instance ToQuery InstanceStatusDetails where
    toQuery InstanceStatusDetails{..} = mconcat
        [ "ImpairedSince" =? _isdImpairedSince
        , "Name"          =? _isdName
        , "Status"        =? _isdStatus
        ]

data IamInstanceProfile = IamInstanceProfile
    { _iipArn :: Maybe Text
    , _iipId  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'IamInstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iipArn' @::@ 'Maybe' 'Text'
--
-- * 'iipId' @::@ 'Maybe' 'Text'
--
iamInstanceProfile :: IamInstanceProfile
iamInstanceProfile = IamInstanceProfile
    { _iipArn = Nothing
    , _iipId  = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iipArn :: Lens' IamInstanceProfile (Maybe Text)
iipArn = lens _iipArn (\s a -> s { _iipArn = a })

-- | The ID of the instance profile.
iipId :: Lens' IamInstanceProfile (Maybe Text)
iipId = lens _iipId (\s a -> s { _iipId = a })

instance FromXML IamInstanceProfile where
    parseXML x = IamInstanceProfile
        <$> x .@? "arn"
        <*> x .@? "id"

instance ToQuery IamInstanceProfile where
    toQuery IamInstanceProfile{..} = mconcat
        [ "Arn" =? _iipArn
        , "Id"  =? _iipId
        ]

data UnsuccessfulItem = UnsuccessfulItem
    { _uiError      :: UnsuccessfulItemError
    , _uiResourceId :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'UnsuccessfulItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiError' @::@ 'UnsuccessfulItemError'
--
-- * 'uiResourceId' @::@ 'Maybe' 'Text'
--
unsuccessfulItem :: UnsuccessfulItemError -- ^ 'uiError'
                 -> UnsuccessfulItem
unsuccessfulItem p1 = UnsuccessfulItem
    { _uiError      = p1
    , _uiResourceId = Nothing
    }

-- | Information about the error.
uiError :: Lens' UnsuccessfulItem UnsuccessfulItemError
uiError = lens _uiError (\s a -> s { _uiError = a })

-- | The ID of the resource.
uiResourceId :: Lens' UnsuccessfulItem (Maybe Text)
uiResourceId = lens _uiResourceId (\s a -> s { _uiResourceId = a })

instance FromXML UnsuccessfulItem where
    parseXML x = UnsuccessfulItem
        <$> x .@  "error"
        <*> x .@? "resourceId"

instance ToQuery UnsuccessfulItem where
    toQuery UnsuccessfulItem{..} = mconcat
        [ "Error"      =? _uiError
        , "ResourceId" =? _uiResourceId
        ]

data InternetGatewayAttachment = InternetGatewayAttachment
    { _igaState :: AttachmentStatus
    , _igaVpcId :: Text
    } deriving (Eq, Read, Show)

-- | 'InternetGatewayAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igaState' @::@ 'AttachmentStatus'
--
-- * 'igaVpcId' @::@ 'Text'
--
internetGatewayAttachment :: Text -- ^ 'igaVpcId'
                          -> AttachmentStatus -- ^ 'igaState'
                          -> InternetGatewayAttachment
internetGatewayAttachment p1 p2 = InternetGatewayAttachment
    { _igaVpcId = p1
    , _igaState = p2
    }

-- | The current state of the attachment.
igaState :: Lens' InternetGatewayAttachment AttachmentStatus
igaState = lens _igaState (\s a -> s { _igaState = a })

-- | The ID of the VPC.
igaVpcId :: Lens' InternetGatewayAttachment Text
igaVpcId = lens _igaVpcId (\s a -> s { _igaVpcId = a })

instance FromXML InternetGatewayAttachment where
    parseXML x = InternetGatewayAttachment
        <$> x .@  "state"
        <*> x .@  "vpcId"

instance ToQuery InternetGatewayAttachment where
    toQuery InternetGatewayAttachment{..} = mconcat
        [ "State" =? _igaState
        , "VpcId" =? _igaVpcId
        ]

data AddressStatus
    = InClassic      -- ^ InClassic
    | InVpc          -- ^ InVpc
    | MoveInProgress -- ^ MoveInProgress
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AddressStatus

instance FromText AddressStatus where
    parser = takeLowerText >>= \case
        "inclassic"      -> pure InClassic
        "invpc"          -> pure InVpc
        "moveinprogress" -> pure MoveInProgress
        e                -> fail $
            "Failure parsing AddressStatus from " ++ show e

instance ToText AddressStatus where
    toText = \case
        InClassic      -> "InClassic"
        InVpc          -> "InVpc"
        MoveInProgress -> "MoveInProgress"

instance ToByteString AddressStatus
instance ToHeader     AddressStatus
instance ToQuery      AddressStatus

instance FromXML AddressStatus where
    parseXML = parseXMLText "AddressStatus"

data ReservedInstanceState
    = RISActive         -- ^ active
    | RISPaymentFailed  -- ^ payment-failed
    | RISPaymentPending -- ^ payment-pending
    | RISRetired        -- ^ retired
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReservedInstanceState

instance FromText ReservedInstanceState where
    parser = takeLowerText >>= \case
        "active"          -> pure RISActive
        "payment-failed"  -> pure RISPaymentFailed
        "payment-pending" -> pure RISPaymentPending
        "retired"         -> pure RISRetired
        e                 -> fail $
            "Failure parsing ReservedInstanceState from " ++ show e

instance ToText ReservedInstanceState where
    toText = \case
        RISActive         -> "active"
        RISPaymentFailed  -> "payment-failed"
        RISPaymentPending -> "payment-pending"
        RISRetired        -> "retired"

instance ToByteString ReservedInstanceState
instance ToHeader     ReservedInstanceState
instance ToQuery      ReservedInstanceState

instance FromXML ReservedInstanceState where
    parseXML = parseXMLText "ReservedInstanceState"

data InstanceAttributeName
    = IANInstanceBlockDeviceMapping                -- ^ blockDeviceMapping
    | IANInstanceDisableApiTermination             -- ^ disableApiTermination
    | IANInstanceEbsOptimized                      -- ^ ebsOptimized
    | IANInstanceGroupSet                          -- ^ groupSet
    | IANInstanceInstanceInitiatedShutdownBehavior -- ^ instanceInitiatedShutdownBehavior
    | IANInstanceInstanceType                      -- ^ instanceType
    | IANInstanceKernel                            -- ^ kernel
    | IANInstanceProductCodes                      -- ^ productCodes
    | IANInstanceRamdisk                           -- ^ ramdisk
    | IANInstanceRootDeviceName                    -- ^ rootDeviceName
    | IANInstanceSourceDestCheck                   -- ^ sourceDestCheck
    | IANInstanceSriovNetSupport                   -- ^ sriovNetSupport
    | IANInstanceUserData                          -- ^ userData
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable InstanceAttributeName

instance FromText InstanceAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping"                -> pure IANInstanceBlockDeviceMapping
        "disableapitermination"             -> pure IANInstanceDisableApiTermination
        "ebsoptimized"                      -> pure IANInstanceEbsOptimized
        "groupset"                          -> pure IANInstanceGroupSet
        "instanceinitiatedshutdownbehavior" -> pure IANInstanceInstanceInitiatedShutdownBehavior
        "instancetype"                      -> pure IANInstanceInstanceType
        "kernel"                            -> pure IANInstanceKernel
        "productcodes"                      -> pure IANInstanceProductCodes
        "ramdisk"                           -> pure IANInstanceRamdisk
        "rootdevicename"                    -> pure IANInstanceRootDeviceName
        "sourcedestcheck"                   -> pure IANInstanceSourceDestCheck
        "sriovnetsupport"                   -> pure IANInstanceSriovNetSupport
        "userdata"                          -> pure IANInstanceUserData
        e                                   -> fail $
            "Failure parsing InstanceAttributeName from " ++ show e

instance ToText InstanceAttributeName where
    toText = \case
        IANInstanceBlockDeviceMapping                -> "blockDeviceMapping"
        IANInstanceDisableApiTermination             -> "disableApiTermination"
        IANInstanceEbsOptimized                      -> "ebsOptimized"
        IANInstanceGroupSet                          -> "groupSet"
        IANInstanceInstanceInitiatedShutdownBehavior -> "instanceInitiatedShutdownBehavior"
        IANInstanceInstanceType                      -> "instanceType"
        IANInstanceKernel                            -> "kernel"
        IANInstanceProductCodes                      -> "productCodes"
        IANInstanceRamdisk                           -> "ramdisk"
        IANInstanceRootDeviceName                    -> "rootDeviceName"
        IANInstanceSourceDestCheck                   -> "sourceDestCheck"
        IANInstanceSriovNetSupport                   -> "sriovNetSupport"
        IANInstanceUserData                          -> "userData"

instance ToByteString InstanceAttributeName
instance ToHeader     InstanceAttributeName
instance ToQuery      InstanceAttributeName

instance FromXML InstanceAttributeName where
    parseXML = parseXMLText "InstanceAttributeName"

data IpPermission = IpPermission
    { _ipFromPort         :: Maybe Int
    , _ipIpProtocol       :: Text
    , _ipIpRanges         :: List "item" IpRange
    , _ipPrefixListIds    :: List "item" PrefixListId
    , _ipToPort           :: Maybe Int
    , _ipUserIdGroupPairs :: List "item" UserIdGroupPair
    } deriving (Eq, Read, Show)

-- | 'IpPermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipFromPort' @::@ 'Maybe' 'Int'
--
-- * 'ipIpProtocol' @::@ 'Text'
--
-- * 'ipIpRanges' @::@ ['IpRange']
--
-- * 'ipPrefixListIds' @::@ ['PrefixListId']
--
-- * 'ipToPort' @::@ 'Maybe' 'Int'
--
-- * 'ipUserIdGroupPairs' @::@ ['UserIdGroupPair']
--
ipPermission :: Text -- ^ 'ipIpProtocol'
             -> IpPermission
ipPermission p1 = IpPermission
    { _ipIpProtocol       = p1
    , _ipFromPort         = Nothing
    , _ipToPort           = Nothing
    , _ipUserIdGroupPairs = mempty
    , _ipIpRanges         = mempty
    , _ipPrefixListIds    = mempty
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP type
-- number. A value of '-1' indicates all ICMP types.
ipFromPort :: Lens' IpPermission (Maybe Int)
ipFromPort = lens _ipFromPort (\s a -> s { _ipFromPort = a })

-- | The protocol.
--
-- When you call 'DescribeSecurityGroups', the protocol value returned is the
-- number. Exception: For TCP, UDP, and ICMP, the value returned is the name
-- (for example, 'tcp', 'udp', or 'icmp'). For a list of protocol numbers, see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>. (VPC only) When you call 'AuthorizeSecurityGroupIngress', you can use '-1' to
-- specify all.
ipIpProtocol :: Lens' IpPermission Text
ipIpProtocol = lens _ipIpProtocol (\s a -> s { _ipIpProtocol = a })

-- | One or more IP ranges.
ipIpRanges :: Lens' IpPermission [IpRange]
ipIpRanges = lens _ipIpRanges (\s a -> s { _ipIpRanges = a }) . _List

-- | (Valid for 'AuthorizeSecurityGroupEgress', 'RevokeSecurityGroupEgress' and 'DescribeSecurityGroups' only) One or more prefix list IDs for an AWS service. In an 'AuthorizeSecurityGroupEgress' request, this is the AWS service that you want to access through a VPC
-- endpoint from instances associated with the security group.
ipPrefixListIds :: Lens' IpPermission [PrefixListId]
ipPrefixListIds = lens _ipPrefixListIds (\s a -> s { _ipPrefixListIds = a }) . _List

-- | The end of port range for the TCP and UDP protocols, or an ICMP code. A value
-- of '-1' indicates all ICMP codes for the specified ICMP type.
ipToPort :: Lens' IpPermission (Maybe Int)
ipToPort = lens _ipToPort (\s a -> s { _ipToPort = a })

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IpPermission [UserIdGroupPair]
ipUserIdGroupPairs =
    lens _ipUserIdGroupPairs (\s a -> s { _ipUserIdGroupPairs = a })
        . _List

instance FromXML IpPermission where
    parseXML x = IpPermission
        <$> x .@? "fromPort"
        <*> x .@  "ipProtocol"
        <*> x .@? "ipRanges" .!@ mempty
        <*> x .@? "prefixListIds" .!@ mempty
        <*> x .@? "toPort"
        <*> x .@? "groups" .!@ mempty

instance ToQuery IpPermission where
    toQuery IpPermission{..} = mconcat
        [ "FromPort"      =? _ipFromPort
        , "IpProtocol"    =? _ipIpProtocol
        , "IpRanges"      `toQueryList` _ipIpRanges
        , "PrefixListIds" `toQueryList` _ipPrefixListIds
        , "ToPort"        =? _ipToPort
        , "Groups"        `toQueryList` _ipUserIdGroupPairs
        ]

data ConversionTaskState
    = CTSActive     -- ^ active
    | CTSCancelled  -- ^ cancelled
    | CTSCancelling -- ^ cancelling
    | CTSCompleted  -- ^ completed
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ConversionTaskState

instance FromText ConversionTaskState where
    parser = takeLowerText >>= \case
        "active"     -> pure CTSActive
        "cancelled"  -> pure CTSCancelled
        "cancelling" -> pure CTSCancelling
        "completed"  -> pure CTSCompleted
        e            -> fail $
            "Failure parsing ConversionTaskState from " ++ show e

instance ToText ConversionTaskState where
    toText = \case
        CTSActive     -> "active"
        CTSCancelled  -> "cancelled"
        CTSCancelling -> "cancelling"
        CTSCompleted  -> "completed"

instance ToByteString ConversionTaskState
instance ToHeader     ConversionTaskState
instance ToQuery      ConversionTaskState

instance FromXML ConversionTaskState where
    parseXML = parseXMLText "ConversionTaskState"

data DiskImage = DiskImage
    { _diDescription :: Maybe Text
    , _diImage       :: Maybe DiskImageDetail
    , _diVolume      :: Maybe VolumeDetail
    } deriving (Eq, Read, Show)

-- | 'DiskImage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDescription' @::@ 'Maybe' 'Text'
--
-- * 'diImage' @::@ 'Maybe' 'DiskImageDetail'
--
-- * 'diVolume' @::@ 'Maybe' 'VolumeDetail'
--
diskImage :: DiskImage
diskImage = DiskImage
    { _diImage       = Nothing
    , _diDescription = Nothing
    , _diVolume      = Nothing
    }

-- | A description of the disk image.
diDescription :: Lens' DiskImage (Maybe Text)
diDescription = lens _diDescription (\s a -> s { _diDescription = a })

-- | Information about the disk image.
diImage :: Lens' DiskImage (Maybe DiskImageDetail)
diImage = lens _diImage (\s a -> s { _diImage = a })

-- | Information about the volume.
diVolume :: Lens' DiskImage (Maybe VolumeDetail)
diVolume = lens _diVolume (\s a -> s { _diVolume = a })

instance FromXML DiskImage where
    parseXML x = DiskImage
        <$> x .@? "Description"
        <*> x .@? "Image"
        <*> x .@? "Volume"

instance ToQuery DiskImage where
    toQuery DiskImage{..} = mconcat
        [ "Description" =? _diDescription
        , "Image"       =? _diImage
        , "Volume"      =? _diVolume
        ]

data Tenancy
    = Dedicated -- ^ dedicated
    | Default'  -- ^ default
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Tenancy

instance FromText Tenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure Dedicated
        "default"   -> pure Default'
        e           -> fail $
            "Failure parsing Tenancy from " ++ show e

instance ToText Tenancy where
    toText = \case
        Dedicated -> "dedicated"
        Default'  -> "default"

instance ToByteString Tenancy
instance ToHeader     Tenancy
instance ToQuery      Tenancy

instance FromXML Tenancy where
    parseXML = parseXMLText "Tenancy"

data UnsuccessfulItemError = UnsuccessfulItemError
    { _uieCode    :: Text
    , _uieMessage :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UnsuccessfulItemError' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uieCode' @::@ 'Text'
--
-- * 'uieMessage' @::@ 'Text'
--
unsuccessfulItemError :: Text -- ^ 'uieCode'
                      -> Text -- ^ 'uieMessage'
                      -> UnsuccessfulItemError
unsuccessfulItemError p1 p2 = UnsuccessfulItemError
    { _uieCode    = p1
    , _uieMessage = p2
    }

-- | The error code.
uieCode :: Lens' UnsuccessfulItemError Text
uieCode = lens _uieCode (\s a -> s { _uieCode = a })

-- | The error message accompanying the error code.
uieMessage :: Lens' UnsuccessfulItemError Text
uieMessage = lens _uieMessage (\s a -> s { _uieMessage = a })

instance FromXML UnsuccessfulItemError where
    parseXML x = UnsuccessfulItemError
        <$> x .@  "code"
        <*> x .@  "message"

instance ToQuery UnsuccessfulItemError where
    toQuery UnsuccessfulItemError{..} = mconcat
        [ "Code"    =? _uieCode
        , "Message" =? _uieMessage
        ]

data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode    :: Maybe Text
    , _vpcsrMessage :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'VpcPeeringConnectionStateReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcsrCode' @::@ 'Maybe' 'Text'
--
-- * 'vpcsrMessage' @::@ 'Maybe' 'Text'
--
vpcPeeringConnectionStateReason :: VpcPeeringConnectionStateReason
vpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason
    { _vpcsrCode    = Nothing
    , _vpcsrMessage = Nothing
    }

-- | The status of the VPC peering connection.
vpcsrCode :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrCode = lens _vpcsrCode (\s a -> s { _vpcsrCode = a })

-- | A message that provides more information about the status, if applicable.
vpcsrMessage :: Lens' VpcPeeringConnectionStateReason (Maybe Text)
vpcsrMessage = lens _vpcsrMessage (\s a -> s { _vpcsrMessage = a })

instance FromXML VpcPeeringConnectionStateReason where
    parseXML x = VpcPeeringConnectionStateReason
        <$> x .@? "code"
        <*> x .@? "message"

instance ToQuery VpcPeeringConnectionStateReason where
    toQuery VpcPeeringConnectionStateReason{..} = mconcat
        [ "Code"    =? _vpcsrCode
        , "Message" =? _vpcsrMessage
        ]

data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn  :: Maybe Text
    , _iipsName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'IamInstanceProfileSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iipsArn' @::@ 'Maybe' 'Text'
--
-- * 'iipsName' @::@ 'Maybe' 'Text'
--
iamInstanceProfileSpecification :: IamInstanceProfileSpecification
iamInstanceProfileSpecification = IamInstanceProfileSpecification
    { _iipsArn  = Nothing
    , _iipsName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iipsArn :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsArn = lens _iipsArn (\s a -> s { _iipsArn = a })

-- | The name of the instance profile.
iipsName :: Lens' IamInstanceProfileSpecification (Maybe Text)
iipsName = lens _iipsName (\s a -> s { _iipsName = a })

instance FromXML IamInstanceProfileSpecification where
    parseXML x = IamInstanceProfileSpecification
        <$> x .@? "arn"
        <*> x .@? "name"

instance ToQuery IamInstanceProfileSpecification where
    toQuery IamInstanceProfileSpecification{..} = mconcat
        [ "Arn"  =? _iipsArn
        , "Name" =? _iipsName
        ]

data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { _ivtdAvailabilityZone :: Text
    , _ivtdBytesConverted   :: Integer
    , _ivtdDescription      :: Maybe Text
    , _ivtdImage            :: DiskImageDescription
    , _ivtdVolume           :: DiskImageVolumeDescription
    } deriving (Eq, Read, Show)

-- | 'ImportVolumeTaskDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivtdAvailabilityZone' @::@ 'Text'
--
-- * 'ivtdBytesConverted' @::@ 'Integer'
--
-- * 'ivtdDescription' @::@ 'Maybe' 'Text'
--
-- * 'ivtdImage' @::@ 'DiskImageDescription'
--
-- * 'ivtdVolume' @::@ 'DiskImageVolumeDescription'
--
importVolumeTaskDetails :: Integer -- ^ 'ivtdBytesConverted'
                        -> Text -- ^ 'ivtdAvailabilityZone'
                        -> DiskImageDescription -- ^ 'ivtdImage'
                        -> DiskImageVolumeDescription -- ^ 'ivtdVolume'
                        -> ImportVolumeTaskDetails
importVolumeTaskDetails p1 p2 p3 p4 = ImportVolumeTaskDetails
    { _ivtdBytesConverted   = p1
    , _ivtdAvailabilityZone = p2
    , _ivtdImage            = p3
    , _ivtdVolume           = p4
    , _ivtdDescription      = Nothing
    }

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails Text
ivtdAvailabilityZone =
    lens _ivtdAvailabilityZone (\s a -> s { _ivtdAvailabilityZone = a })

-- | The number of bytes converted so far.
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails Integer
ivtdBytesConverted =
    lens _ivtdBytesConverted (\s a -> s { _ivtdBytesConverted = a })

-- | The description you provided when starting the import volume task.
ivtdDescription :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdDescription = lens _ivtdDescription (\s a -> s { _ivtdDescription = a })

-- | The image.
ivtdImage :: Lens' ImportVolumeTaskDetails DiskImageDescription
ivtdImage = lens _ivtdImage (\s a -> s { _ivtdImage = a })

-- | The volume.
ivtdVolume :: Lens' ImportVolumeTaskDetails DiskImageVolumeDescription
ivtdVolume = lens _ivtdVolume (\s a -> s { _ivtdVolume = a })

instance FromXML ImportVolumeTaskDetails where
    parseXML x = ImportVolumeTaskDetails
        <$> x .@  "availabilityZone"
        <*> x .@  "bytesConverted"
        <*> x .@? "description"
        <*> x .@  "image"
        <*> x .@  "volume"

instance ToQuery ImportVolumeTaskDetails where
    toQuery ImportVolumeTaskDetails{..} = mconcat
        [ "AvailabilityZone" =? _ivtdAvailabilityZone
        , "BytesConverted"   =? _ivtdBytesConverted
        , "Description"      =? _ivtdDescription
        , "Image"            =? _ivtdImage
        , "Volume"           =? _ivtdVolume
        ]

data PlacementStrategy
    = Cluster -- ^ cluster
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable PlacementStrategy

instance FromText PlacementStrategy where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        e         -> fail $
            "Failure parsing PlacementStrategy from " ++ show e

instance ToText PlacementStrategy where
    toText Cluster = "cluster"

instance ToByteString PlacementStrategy
instance ToHeader     PlacementStrategy
instance ToQuery      PlacementStrategy

instance FromXML PlacementStrategy where
    parseXML = parseXMLText "PlacementStrategy"

data InstanceNetworkInterface = InstanceNetworkInterface
    { _iniAssociation        :: Maybe InstanceNetworkInterfaceAssociation
    , _iniAttachment         :: Maybe InstanceNetworkInterfaceAttachment
    , _iniDescription        :: Maybe Text
    , _iniGroups             :: List "item" GroupIdentifier
    , _iniMacAddress         :: Maybe Text
    , _iniNetworkInterfaceId :: Maybe Text
    , _iniOwnerId            :: Maybe Text
    , _iniPrivateDnsName     :: Maybe Text
    , _iniPrivateIpAddress   :: Maybe Text
    , _iniPrivateIpAddresses :: List "item" InstancePrivateIpAddress
    , _iniSourceDestCheck    :: Maybe Bool
    , _iniStatus             :: Maybe NetworkInterfaceStatus
    , _iniSubnetId           :: Maybe Text
    , _iniVpcId              :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'InstanceNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniAssociation' @::@ 'Maybe' 'InstanceNetworkInterfaceAssociation'
--
-- * 'iniAttachment' @::@ 'Maybe' 'InstanceNetworkInterfaceAttachment'
--
-- * 'iniDescription' @::@ 'Maybe' 'Text'
--
-- * 'iniGroups' @::@ ['GroupIdentifier']
--
-- * 'iniMacAddress' @::@ 'Maybe' 'Text'
--
-- * 'iniNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'iniOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'iniPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'iniPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'iniPrivateIpAddresses' @::@ ['InstancePrivateIpAddress']
--
-- * 'iniSourceDestCheck' @::@ 'Maybe' 'Bool'
--
-- * 'iniStatus' @::@ 'Maybe' 'NetworkInterfaceStatus'
--
-- * 'iniSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'iniVpcId' @::@ 'Maybe' 'Text'
--
instanceNetworkInterface :: InstanceNetworkInterface
instanceNetworkInterface = InstanceNetworkInterface
    { _iniNetworkInterfaceId = Nothing
    , _iniSubnetId           = Nothing
    , _iniVpcId              = Nothing
    , _iniDescription        = Nothing
    , _iniOwnerId            = Nothing
    , _iniStatus             = Nothing
    , _iniMacAddress         = Nothing
    , _iniPrivateIpAddress   = Nothing
    , _iniPrivateDnsName     = Nothing
    , _iniSourceDestCheck    = Nothing
    , _iniGroups             = mempty
    , _iniAttachment         = Nothing
    , _iniAssociation        = Nothing
    , _iniPrivateIpAddresses = mempty
    }

-- | The association information for an Elastic IP associated with the network
-- interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = lens _iniAssociation (\s a -> s { _iniAssociation = a })

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = lens _iniAttachment (\s a -> s { _iniAttachment = a })

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription = lens _iniDescription (\s a -> s { _iniDescription = a })

-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface [GroupIdentifier]
iniGroups = lens _iniGroups (\s a -> s { _iniGroups = a }) . _List

-- | The MAC address.
iniMacAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniMacAddress = lens _iniMacAddress (\s a -> s { _iniMacAddress = a })

-- | The ID of the network interface.
iniNetworkInterfaceId :: Lens' InstanceNetworkInterface (Maybe Text)
iniNetworkInterfaceId =
    lens _iniNetworkInterfaceId (\s a -> s { _iniNetworkInterfaceId = a })

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId = lens _iniOwnerId (\s a -> s { _iniOwnerId = a })

-- | The private DNS name.
iniPrivateDnsName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDnsName =
    lens _iniPrivateDnsName (\s a -> s { _iniPrivateDnsName = a })

-- | The IP address of the network interface within the subnet.
iniPrivateIpAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIpAddress =
    lens _iniPrivateIpAddress (\s a -> s { _iniPrivateIpAddress = a })

-- | The private IP addresses associated with the network interface.
iniPrivateIpAddresses :: Lens' InstanceNetworkInterface [InstancePrivateIpAddress]
iniPrivateIpAddresses =
    lens _iniPrivateIpAddresses (\s a -> s { _iniPrivateIpAddresses = a })
        . _List

-- | Indicates whether to validate network traffic to or from this network
-- interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck =
    lens _iniSourceDestCheck (\s a -> s { _iniSourceDestCheck = a })

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe NetworkInterfaceStatus)
iniStatus = lens _iniStatus (\s a -> s { _iniStatus = a })

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId = lens _iniSubnetId (\s a -> s { _iniSubnetId = a })

-- | The ID of the VPC.
iniVpcId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVpcId = lens _iniVpcId (\s a -> s { _iniVpcId = a })

instance FromXML InstanceNetworkInterface where
    parseXML x = InstanceNetworkInterface
        <$> x .@? "association"
        <*> x .@? "attachment"
        <*> x .@? "description"
        <*> x .@? "groupSet" .!@ mempty
        <*> x .@? "macAddress"
        <*> x .@? "networkInterfaceId"
        <*> x .@? "ownerId"
        <*> x .@? "privateDnsName"
        <*> x .@? "privateIpAddress"
        <*> x .@? "privateIpAddressesSet" .!@ mempty
        <*> x .@? "sourceDestCheck"
        <*> x .@? "status"
        <*> x .@? "subnetId"
        <*> x .@? "vpcId"

instance ToQuery InstanceNetworkInterface where
    toQuery InstanceNetworkInterface{..} = mconcat
        [ "Association"           =? _iniAssociation
        , "Attachment"            =? _iniAttachment
        , "Description"           =? _iniDescription
        , "GroupSet"              `toQueryList` _iniGroups
        , "MacAddress"            =? _iniMacAddress
        , "NetworkInterfaceId"    =? _iniNetworkInterfaceId
        , "OwnerId"               =? _iniOwnerId
        , "PrivateDnsName"        =? _iniPrivateDnsName
        , "PrivateIpAddress"      =? _iniPrivateIpAddress
        , "PrivateIpAddressesSet" `toQueryList` _iniPrivateIpAddresses
        , "SourceDestCheck"       =? _iniSourceDestCheck
        , "Status"                =? _iniStatus
        , "SubnetId"              =? _iniSubnetId
        , "VpcId"                 =? _iniVpcId
        ]

data VolumeStatusAction = VolumeStatusAction
    { _vsaCode        :: Maybe Text
    , _vsaDescription :: Maybe Text
    , _vsaEventId     :: Maybe Text
    , _vsaEventType   :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'VolumeStatusAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsaCode' @::@ 'Maybe' 'Text'
--
-- * 'vsaDescription' @::@ 'Maybe' 'Text'
--
-- * 'vsaEventId' @::@ 'Maybe' 'Text'
--
-- * 'vsaEventType' @::@ 'Maybe' 'Text'
--
volumeStatusAction :: VolumeStatusAction
volumeStatusAction = VolumeStatusAction
    { _vsaCode        = Nothing
    , _vsaDescription = Nothing
    , _vsaEventType   = Nothing
    , _vsaEventId     = Nothing
    }

-- | The code identifying the operation, for example, 'enable-volume-io'.
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode = lens _vsaCode (\s a -> s { _vsaCode = a })

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription = lens _vsaDescription (\s a -> s { _vsaDescription = a })

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId = lens _vsaEventId (\s a -> s { _vsaEventId = a })

-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType = lens _vsaEventType (\s a -> s { _vsaEventType = a })

instance FromXML VolumeStatusAction where
    parseXML x = VolumeStatusAction
        <$> x .@? "code"
        <*> x .@? "description"
        <*> x .@? "eventId"
        <*> x .@? "eventType"

instance ToQuery VolumeStatusAction where
    toQuery VolumeStatusAction{..} = mconcat
        [ "Code"        =? _vsaCode
        , "Description" =? _vsaDescription
        , "EventId"     =? _vsaEventId
        , "EventType"   =? _vsaEventType
        ]

data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock :: Maybe Text
    , _vpcviOwnerId   :: Maybe Text
    , _vpcviVpcId     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'VpcPeeringConnectionVpcInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcviCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'vpcviOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'vpcviVpcId' @::@ 'Maybe' 'Text'
--
vpcPeeringConnectionVpcInfo :: VpcPeeringConnectionVpcInfo
vpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo
    { _vpcviCidrBlock = Nothing
    , _vpcviOwnerId   = Nothing
    , _vpcviVpcId     = Nothing
    }

-- | The CIDR block for the VPC.
vpcviCidrBlock :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviCidrBlock = lens _vpcviCidrBlock (\s a -> s { _vpcviCidrBlock = a })

-- | The AWS account ID of the VPC owner.
vpcviOwnerId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviOwnerId = lens _vpcviOwnerId (\s a -> s { _vpcviOwnerId = a })

-- | The ID of the VPC.
vpcviVpcId :: Lens' VpcPeeringConnectionVpcInfo (Maybe Text)
vpcviVpcId = lens _vpcviVpcId (\s a -> s { _vpcviVpcId = a })

instance FromXML VpcPeeringConnectionVpcInfo where
    parseXML x = VpcPeeringConnectionVpcInfo
        <$> x .@? "cidrBlock"
        <*> x .@? "ownerId"
        <*> x .@? "vpcId"

instance ToQuery VpcPeeringConnectionVpcInfo where
    toQuery VpcPeeringConnectionVpcInfo{..} = mconcat
        [ "CidrBlock" =? _vpcviCidrBlock
        , "OwnerId"   =? _vpcviOwnerId
        , "VpcId"     =? _vpcviVpcId
        ]

data UserBucketDetails = UserBucketDetails
    { _ubdS3Bucket :: Maybe Text
    , _ubdS3Key    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UserBucketDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubdS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'ubdS3Key' @::@ 'Maybe' 'Text'
--
userBucketDetails :: UserBucketDetails
userBucketDetails = UserBucketDetails
    { _ubdS3Bucket = Nothing
    , _ubdS3Key    = Nothing
    }

-- | The S3 bucket from which the disk image was created.
ubdS3Bucket :: Lens' UserBucketDetails (Maybe Text)
ubdS3Bucket = lens _ubdS3Bucket (\s a -> s { _ubdS3Bucket = a })

-- | The key from which the disk image was created.
ubdS3Key :: Lens' UserBucketDetails (Maybe Text)
ubdS3Key = lens _ubdS3Key (\s a -> s { _ubdS3Key = a })

instance FromXML UserBucketDetails where
    parseXML x = UserBucketDetails
        <$> x .@? "s3Bucket"
        <*> x .@? "s3Key"

instance ToQuery UserBucketDetails where
    toQuery UserBucketDetails{..} = mconcat
        [ "S3Bucket" =? _ubdS3Bucket
        , "S3Key"    =? _ubdS3Key
        ]

data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount       :: Maybe Double
    , _rilpCurrencyCode :: Maybe CurrencyCodeValues
    } deriving (Eq, Read, Show)

-- | 'ReservedInstanceLimitPrice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rilpAmount' @::@ 'Maybe' 'Double'
--
-- * 'rilpCurrencyCode' @::@ 'Maybe' 'CurrencyCodeValues'
--
reservedInstanceLimitPrice :: ReservedInstanceLimitPrice
reservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { _rilpAmount       = Nothing
    , _rilpCurrencyCode = Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit price
-- on the total order (instanceCount * price).
rilpAmount :: Lens' ReservedInstanceLimitPrice (Maybe Double)
rilpAmount = lens _rilpAmount (\s a -> s { _rilpAmount = a })

-- | The currency in which the 'limitPrice' amount is specified. At this time, the
-- only supported currency is 'USD'.
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe CurrencyCodeValues)
rilpCurrencyCode = lens _rilpCurrencyCode (\s a -> s { _rilpCurrencyCode = a })

instance FromXML ReservedInstanceLimitPrice where
    parseXML x = ReservedInstanceLimitPrice
        <$> x .@? "amount"
        <*> x .@? "currencyCode"

instance ToQuery ReservedInstanceLimitPrice where
    toQuery ReservedInstanceLimitPrice{..} = mconcat
        [ "Amount"       =? _rilpAmount
        , "CurrencyCode" =? _rilpCurrencyCode
        ]

data Vpc = Vpc
    { _vpcCidrBlock       :: Text
    , _vpcDhcpOptionsId   :: Text
    , _vpcInstanceTenancy :: Tenancy
    , _vpcIsDefault       :: Bool
    , _vpcState           :: VpcState
    , _vpcTags            :: List "item" Tag
    , _vpcVpcId           :: Text
    } deriving (Eq, Read, Show)

-- | 'Vpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcCidrBlock' @::@ 'Text'
--
-- * 'vpcDhcpOptionsId' @::@ 'Text'
--
-- * 'vpcInstanceTenancy' @::@ 'Tenancy'
--
-- * 'vpcIsDefault' @::@ 'Bool'
--
-- * 'vpcState' @::@ 'VpcState'
--
-- * 'vpcTags' @::@ ['Tag']
--
-- * 'vpcVpcId' @::@ 'Text'
--
vpc :: Text -- ^ 'vpcVpcId'
    -> VpcState -- ^ 'vpcState'
    -> Text -- ^ 'vpcCidrBlock'
    -> Text -- ^ 'vpcDhcpOptionsId'
    -> Tenancy -- ^ 'vpcInstanceTenancy'
    -> Bool -- ^ 'vpcIsDefault'
    -> Vpc
vpc p1 p2 p3 p4 p5 p6 = Vpc
    { _vpcVpcId           = p1
    , _vpcState           = p2
    , _vpcCidrBlock       = p3
    , _vpcDhcpOptionsId   = p4
    , _vpcInstanceTenancy = p5
    , _vpcIsDefault       = p6
    , _vpcTags            = mempty
    }

-- | The CIDR block for the VPC.
vpcCidrBlock :: Lens' Vpc Text
vpcCidrBlock = lens _vpcCidrBlock (\s a -> s { _vpcCidrBlock = a })

-- | The ID of the set of DHCP options you've associated with the VPC (or 'default'
-- if the default options are associated with the VPC).
vpcDhcpOptionsId :: Lens' Vpc Text
vpcDhcpOptionsId = lens _vpcDhcpOptionsId (\s a -> s { _vpcDhcpOptionsId = a })

-- | The allowed tenancy of instances launched into the VPC.
vpcInstanceTenancy :: Lens' Vpc Tenancy
vpcInstanceTenancy =
    lens _vpcInstanceTenancy (\s a -> s { _vpcInstanceTenancy = a })

-- | Indicates whether the VPC is the default VPC.
vpcIsDefault :: Lens' Vpc Bool
vpcIsDefault = lens _vpcIsDefault (\s a -> s { _vpcIsDefault = a })

-- | The current state of the VPC.
vpcState :: Lens' Vpc VpcState
vpcState = lens _vpcState (\s a -> s { _vpcState = a })

-- | Any tags assigned to the VPC.
vpcTags :: Lens' Vpc [Tag]
vpcTags = lens _vpcTags (\s a -> s { _vpcTags = a }) . _List

-- | The ID of the VPC.
vpcVpcId :: Lens' Vpc Text
vpcVpcId = lens _vpcVpcId (\s a -> s { _vpcVpcId = a })

instance FromXML Vpc where
    parseXML x = Vpc
        <$> x .@  "cidrBlock"
        <*> x .@  "dhcpOptionsId"
        <*> x .@  "instanceTenancy"
        <*> x .@  "isDefault"
        <*> x .@  "state"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "vpcId"

instance ToQuery Vpc where
    toQuery Vpc{..} = mconcat
        [ "CidrBlock"       =? _vpcCidrBlock
        , "DhcpOptionsId"   =? _vpcDhcpOptionsId
        , "InstanceTenancy" =? _vpcInstanceTenancy
        , "IsDefault"       =? _vpcIsDefault
        , "State"           =? _vpcState
        , "TagSet"          `toQueryList` _vpcTags
        , "VpcId"           =? _vpcVpcId
        ]

data ImageDiskContainer = ImageDiskContainer
    { _idcDescription :: Maybe Text
    , _idcDeviceName  :: Maybe Text
    , _idcFormat      :: Maybe Text
    , _idcSnapshotId  :: Maybe Text
    , _idcUrl         :: Maybe Text
    , _idcUserBucket  :: Maybe UserBucket
    } deriving (Eq, Read, Show)

-- | 'ImageDiskContainer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idcDescription' @::@ 'Maybe' 'Text'
--
-- * 'idcDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'idcFormat' @::@ 'Maybe' 'Text'
--
-- * 'idcSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'idcUrl' @::@ 'Maybe' 'Text'
--
-- * 'idcUserBucket' @::@ 'Maybe' 'UserBucket'
--
imageDiskContainer :: ImageDiskContainer
imageDiskContainer = ImageDiskContainer
    { _idcDescription = Nothing
    , _idcFormat      = Nothing
    , _idcUrl         = Nothing
    , _idcUserBucket  = Nothing
    , _idcDeviceName  = Nothing
    , _idcSnapshotId  = Nothing
    }

-- | The description of the disk image.
idcDescription :: Lens' ImageDiskContainer (Maybe Text)
idcDescription = lens _idcDescription (\s a -> s { _idcDescription = a })

-- | The block device mapping for the disk.
idcDeviceName :: Lens' ImageDiskContainer (Maybe Text)
idcDeviceName = lens _idcDeviceName (\s a -> s { _idcDeviceName = a })

-- | The format of the disk image being imported.
--
-- Valid values: 'RAW' | 'VHD' | 'VMDK' | 'OVA'
idcFormat :: Lens' ImageDiskContainer (Maybe Text)
idcFormat = lens _idcFormat (\s a -> s { _idcFormat = a })

-- | The ID of the EBS snapshot to be used for importing the snapshot.
idcSnapshotId :: Lens' ImageDiskContainer (Maybe Text)
idcSnapshotId = lens _idcSnapshotId (\s a -> s { _idcSnapshotId = a })

-- | The URL to the Amazon S3-based disk image being imported. The URL can either
-- be a https URL (https://..) or an Amazon S3 URL (s3://..)
idcUrl :: Lens' ImageDiskContainer (Maybe Text)
idcUrl = lens _idcUrl (\s a -> s { _idcUrl = a })

-- | The S3 bucket for the disk image.
idcUserBucket :: Lens' ImageDiskContainer (Maybe UserBucket)
idcUserBucket = lens _idcUserBucket (\s a -> s { _idcUserBucket = a })

instance FromXML ImageDiskContainer where
    parseXML x = ImageDiskContainer
        <$> x .@? "Description"
        <*> x .@? "DeviceName"
        <*> x .@? "Format"
        <*> x .@? "SnapshotId"
        <*> x .@? "Url"
        <*> x .@? "UserBucket"

instance ToQuery ImageDiskContainer where
    toQuery ImageDiskContainer{..} = mconcat
        [ "Description" =? _idcDescription
        , "DeviceName"  =? _idcDeviceName
        , "Format"      =? _idcFormat
        , "SnapshotId"  =? _idcSnapshotId
        , "Url"         =? _idcUrl
        , "UserBucket"  =? _idcUserBucket
        ]

data InstanceStatus = InstanceStatus
    { _isAvailabilityZone :: Maybe Text
    , _isEvents           :: List "item" InstanceStatusEvent
    , _isInstanceId       :: Maybe Text
    , _isInstanceState    :: Maybe InstanceState
    , _isInstanceStatus   :: Maybe InstanceStatusSummary
    , _isSystemStatus     :: Maybe InstanceStatusSummary
    } deriving (Eq, Read, Show)

-- | 'InstanceStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'isEvents' @::@ ['InstanceStatusEvent']
--
-- * 'isInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'isInstanceState' @::@ 'Maybe' 'InstanceState'
--
-- * 'isInstanceStatus' @::@ 'Maybe' 'InstanceStatusSummary'
--
-- * 'isSystemStatus' @::@ 'Maybe' 'InstanceStatusSummary'
--
instanceStatus :: InstanceStatus
instanceStatus = InstanceStatus
    { _isInstanceId       = Nothing
    , _isAvailabilityZone = Nothing
    , _isEvents           = mempty
    , _isInstanceState    = Nothing
    , _isSystemStatus     = Nothing
    , _isInstanceStatus   = Nothing
    }

-- | The Availability Zone of the instance.
isAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
isAvailabilityZone =
    lens _isAvailabilityZone (\s a -> s { _isAvailabilityZone = a })

-- | Any scheduled events associated with the instance.
isEvents :: Lens' InstanceStatus [InstanceStatusEvent]
isEvents = lens _isEvents (\s a -> s { _isEvents = a }) . _List

-- | The ID of the instance.
isInstanceId :: Lens' InstanceStatus (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s { _isInstanceId = a })

-- | The intended state of the instance. 'DescribeInstanceStatus' requires that an
-- instance be in the 'running' state.
isInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
isInstanceState = lens _isInstanceState (\s a -> s { _isInstanceState = a })

-- | Reports impaired functionality that stems from issues internal to the
-- instance, such as impaired reachability.
isInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isInstanceStatus = lens _isInstanceStatus (\s a -> s { _isInstanceStatus = a })

-- | Reports impaired functionality that stems from issues related to the systems
-- that support an instance, such as hardware failures and network connectivity
-- problems.
isSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isSystemStatus = lens _isSystemStatus (\s a -> s { _isSystemStatus = a })

instance FromXML InstanceStatus where
    parseXML x = InstanceStatus
        <$> x .@? "availabilityZone"
        <*> x .@? "eventsSet" .!@ mempty
        <*> x .@? "instanceId"
        <*> x .@? "instanceState"
        <*> x .@? "instanceStatus"
        <*> x .@? "systemStatus"

instance ToQuery InstanceStatus where
    toQuery InstanceStatus{..} = mconcat
        [ "AvailabilityZone" =? _isAvailabilityZone
        , "EventsSet"        `toQueryList` _isEvents
        , "InstanceId"       =? _isInstanceId
        , "InstanceState"    =? _isInstanceState
        , "InstanceStatus"   =? _isInstanceStatus
        , "SystemStatus"     =? _isSystemStatus
        ]

data ArchitectureValues
    = I386  -- ^ i386
    | X8664 -- ^ x86_64
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ArchitectureValues

instance FromText ArchitectureValues where
    parser = takeLowerText >>= \case
        "i386"   -> pure I386
        "x86_64" -> pure X8664
        e        -> fail $
            "Failure parsing ArchitectureValues from " ++ show e

instance ToText ArchitectureValues where
    toText = \case
        I386  -> "i386"
        X8664 -> "x86_64"

instance ToByteString ArchitectureValues
instance ToHeader     ArchitectureValues
instance ToQuery      ArchitectureValues

instance FromXML ArchitectureValues where
    parseXML = parseXMLText "ArchitectureValues"

data ReportInstanceReasonCodes
    = InstanceStuckInState     -- ^ instance-stuck-in-state
    | NotAcceptingCredentials  -- ^ not-accepting-credentials
    | Other                    -- ^ other
    | PasswordNotAvailable     -- ^ password-not-available
    | PerformanceEbsVolume     -- ^ performance-ebs-volume
    | PerformanceInstanceStore -- ^ performance-instance-store
    | PerformanceNetwork       -- ^ performance-network
    | PerformanceOther         -- ^ performance-other
    | Unresponsive             -- ^ unresponsive
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReportInstanceReasonCodes

instance FromText ReportInstanceReasonCodes where
    parser = takeLowerText >>= \case
        "instance-stuck-in-state"    -> pure InstanceStuckInState
        "not-accepting-credentials"  -> pure NotAcceptingCredentials
        "other"                      -> pure Other
        "password-not-available"     -> pure PasswordNotAvailable
        "performance-ebs-volume"     -> pure PerformanceEbsVolume
        "performance-instance-store" -> pure PerformanceInstanceStore
        "performance-network"        -> pure PerformanceNetwork
        "performance-other"          -> pure PerformanceOther
        "unresponsive"               -> pure Unresponsive
        e                            -> fail $
            "Failure parsing ReportInstanceReasonCodes from " ++ show e

instance ToText ReportInstanceReasonCodes where
    toText = \case
        InstanceStuckInState     -> "instance-stuck-in-state"
        NotAcceptingCredentials  -> "not-accepting-credentials"
        Other                    -> "other"
        PasswordNotAvailable     -> "password-not-available"
        PerformanceEbsVolume     -> "performance-ebs-volume"
        PerformanceInstanceStore -> "performance-instance-store"
        PerformanceNetwork       -> "performance-network"
        PerformanceOther         -> "performance-other"
        Unresponsive             -> "unresponsive"

instance ToByteString ReportInstanceReasonCodes
instance ToHeader     ReportInstanceReasonCodes
instance ToQuery      ReportInstanceReasonCodes

instance FromXML ReportInstanceReasonCodes where
    parseXML = parseXMLText "ReportInstanceReasonCodes"

data MoveStatus
    = MovingToVpc        -- ^ movingToVpc
    | RestoringToClassic -- ^ restoringToClassic
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable MoveStatus

instance FromText MoveStatus where
    parser = takeLowerText >>= \case
        "movingtovpc"        -> pure MovingToVpc
        "restoringtoclassic" -> pure RestoringToClassic
        e                    -> fail $
            "Failure parsing MoveStatus from " ++ show e

instance ToText MoveStatus where
    toText = \case
        MovingToVpc        -> "movingToVpc"
        RestoringToClassic -> "restoringToClassic"

instance ToByteString MoveStatus
instance ToHeader     MoveStatus
instance ToQuery      MoveStatus

instance FromXML MoveStatus where
    parseXML = parseXMLText "MoveStatus"

data EbsBlockDevice = EbsBlockDevice
    { _ebdDeleteOnTermination :: Maybe Bool
    , _ebdEncrypted           :: Maybe Bool
    , _ebdIops                :: Maybe Int
    , _ebdSnapshotId          :: Maybe Text
    , _ebdVolumeSize          :: Maybe Int
    , _ebdVolumeType          :: Maybe VolumeType
    } deriving (Eq, Read, Show)

-- | 'EbsBlockDevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ebdDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'ebdEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'ebdIops' @::@ 'Maybe' 'Int'
--
-- * 'ebdSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'ebdVolumeSize' @::@ 'Maybe' 'Int'
--
-- * 'ebdVolumeType' @::@ 'Maybe' 'VolumeType'
--
ebsBlockDevice :: EbsBlockDevice
ebsBlockDevice = EbsBlockDevice
    { _ebdSnapshotId          = Nothing
    , _ebdVolumeSize          = Nothing
    , _ebdDeleteOnTermination = Nothing
    , _ebdVolumeType          = Nothing
    , _ebdIops                = Nothing
    , _ebdEncrypted           = Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EbsBlockDevice (Maybe Bool)
ebdDeleteOnTermination =
    lens _ebdDeleteOnTermination (\s a -> s { _ebdDeleteOnTermination = a })

-- | Indicates whether the EBS volume is encrypted. Encrypted Amazon EBS volumes
-- may only be attached to instances that support Amazon EBS encryption.
ebdEncrypted :: Lens' EbsBlockDevice (Maybe Bool)
ebdEncrypted = lens _ebdEncrypted (\s a -> s { _ebdEncrypted = a })

-- | The number of I/O operations per second (IOPS) that the volume supports. For
-- Provisioned IOPS (SSD) volumes, this represents the number of IOPS that are
-- provisioned for the volume. For General Purpose (SSD) volumes, this
-- represents the baseline performance of the volume and the rate at which the
-- volume accumulates I/O credits for bursting. For more information on General
-- Purpose (SSD) baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBSVolume Types> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes and 3
-- to 10000 for General Purpose (SSD) volumes.
--
-- Condition: This parameter is required for requests to create 'io1' volumes; it
-- is not used in requests to create 'standard' or 'gp2' volumes.
ebdIops :: Lens' EbsBlockDevice (Maybe Int)
ebdIops = lens _ebdIops (\s a -> s { _ebdIops = a })

-- | The ID of the snapshot.
ebdSnapshotId :: Lens' EbsBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\s a -> s { _ebdSnapshotId = a })

-- | The size of the volume, in GiB.
--
-- Constraints: '1-1024' for 'standard' volumes, '1-16384' for 'gp2' volumes, and '4-16384' for 'io1' volumes. If you specify a snapshot, the volume size must be equal to
-- or larger than the snapshot size.
--
-- Default: If you're creating the volume from a snapshot and don't specify a
-- volume size, the default is the snapshot size.
ebdVolumeSize :: Lens' EbsBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\s a -> s { _ebdVolumeSize = a })

-- | The volume type. 'gp2' for General Purpose (SSD) volumes, 'io1' for Provisioned
-- IOPS (SSD) volumes, and 'standard' for Magnetic volumes.
--
-- Default: 'standard'
ebdVolumeType :: Lens' EbsBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\s a -> s { _ebdVolumeType = a })

instance FromXML EbsBlockDevice where
    parseXML x = EbsBlockDevice
        <$> x .@? "deleteOnTermination"
        <*> x .@? "encrypted"
        <*> x .@? "iops"
        <*> x .@? "snapshotId"
        <*> x .@? "volumeSize"
        <*> x .@? "volumeType"

instance ToQuery EbsBlockDevice where
    toQuery EbsBlockDevice{..} = mconcat
        [ "DeleteOnTermination" =? _ebdDeleteOnTermination
        , "Encrypted"           =? _ebdEncrypted
        , "Iops"                =? _ebdIops
        , "SnapshotId"          =? _ebdSnapshotId
        , "VolumeSize"          =? _ebdVolumeSize
        , "VolumeType"          =? _ebdVolumeType
        ]

data AccountAttribute = AccountAttribute
    { _aaAttributeName   :: Maybe Text
    , _aaAttributeValues :: List "item" AccountAttributeValue
    } deriving (Eq, Read, Show)

-- | 'AccountAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaAttributeName' @::@ 'Maybe' 'Text'
--
-- * 'aaAttributeValues' @::@ ['AccountAttributeValue']
--
accountAttribute :: AccountAttribute
accountAttribute = AccountAttribute
    { _aaAttributeName   = Nothing
    , _aaAttributeValues = mempty
    }

-- | The name of the account attribute.
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName = lens _aaAttributeName (\s a -> s { _aaAttributeName = a })

-- | One or more values for the account attribute.
aaAttributeValues :: Lens' AccountAttribute [AccountAttributeValue]
aaAttributeValues =
    lens _aaAttributeValues (\s a -> s { _aaAttributeValues = a })
        . _List

instance FromXML AccountAttribute where
    parseXML x = AccountAttribute
        <$> x .@? "attributeName"
        <*> x .@? "attributeValueSet" .!@ mempty

instance ToQuery AccountAttribute where
    toQuery AccountAttribute{..} = mconcat
        [ "AttributeName"     =? _aaAttributeName
        , "AttributeValueSet" `toQueryList` _aaAttributeValues
        ]

data SnapshotDetail = SnapshotDetail
    { _sdDescription   :: Maybe Text
    , _sdDeviceName    :: Maybe Text
    , _sdDiskImageSize :: Maybe Double
    , _sdFormat        :: Maybe Text
    , _sdProgress      :: Maybe Text
    , _sdSnapshotId    :: Maybe Text
    , _sdStatus        :: Maybe Text
    , _sdStatusMessage :: Maybe Text
    , _sdUrl           :: Maybe Text
    , _sdUserBucket    :: Maybe UserBucketDetails
    } deriving (Eq, Read, Show)

-- | 'SnapshotDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdDescription' @::@ 'Maybe' 'Text'
--
-- * 'sdDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'sdDiskImageSize' @::@ 'Maybe' 'Double'
--
-- * 'sdFormat' @::@ 'Maybe' 'Text'
--
-- * 'sdProgress' @::@ 'Maybe' 'Text'
--
-- * 'sdSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'sdStatus' @::@ 'Maybe' 'Text'
--
-- * 'sdStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'sdUrl' @::@ 'Maybe' 'Text'
--
-- * 'sdUserBucket' @::@ 'Maybe' 'UserBucketDetails'
--
snapshotDetail :: SnapshotDetail
snapshotDetail = SnapshotDetail
    { _sdDiskImageSize = Nothing
    , _sdDescription   = Nothing
    , _sdFormat        = Nothing
    , _sdUrl           = Nothing
    , _sdUserBucket    = Nothing
    , _sdDeviceName    = Nothing
    , _sdSnapshotId    = Nothing
    , _sdProgress      = Nothing
    , _sdStatusMessage = Nothing
    , _sdStatus        = Nothing
    }

-- | A description for the snapshot.
sdDescription :: Lens' SnapshotDetail (Maybe Text)
sdDescription = lens _sdDescription (\s a -> s { _sdDescription = a })

-- | The block device mapping for the snapshot.
sdDeviceName :: Lens' SnapshotDetail (Maybe Text)
sdDeviceName = lens _sdDeviceName (\s a -> s { _sdDeviceName = a })

-- | The size of the disk in the snapshot, in GiB.
sdDiskImageSize :: Lens' SnapshotDetail (Maybe Double)
sdDiskImageSize = lens _sdDiskImageSize (\s a -> s { _sdDiskImageSize = a })

-- | The format of the disk image from which the snapshot is created.
sdFormat :: Lens' SnapshotDetail (Maybe Text)
sdFormat = lens _sdFormat (\s a -> s { _sdFormat = a })

-- | The percentage of progress for the task.
sdProgress :: Lens' SnapshotDetail (Maybe Text)
sdProgress = lens _sdProgress (\s a -> s { _sdProgress = a })

-- | The snapshot ID of the disk being imported.
sdSnapshotId :: Lens' SnapshotDetail (Maybe Text)
sdSnapshotId = lens _sdSnapshotId (\s a -> s { _sdSnapshotId = a })

-- | A brief status of the snapshot creation.
sdStatus :: Lens' SnapshotDetail (Maybe Text)
sdStatus = lens _sdStatus (\s a -> s { _sdStatus = a })

-- | A detailed status message for the snapshot creation.
sdStatusMessage :: Lens' SnapshotDetail (Maybe Text)
sdStatusMessage = lens _sdStatusMessage (\s a -> s { _sdStatusMessage = a })

-- | The URL used to access the disk image.
sdUrl :: Lens' SnapshotDetail (Maybe Text)
sdUrl = lens _sdUrl (\s a -> s { _sdUrl = a })

sdUserBucket :: Lens' SnapshotDetail (Maybe UserBucketDetails)
sdUserBucket = lens _sdUserBucket (\s a -> s { _sdUserBucket = a })

instance FromXML SnapshotDetail where
    parseXML x = SnapshotDetail
        <$> x .@? "description"
        <*> x .@? "deviceName"
        <*> x .@? "diskImageSize"
        <*> x .@? "format"
        <*> x .@? "progress"
        <*> x .@? "snapshotId"
        <*> x .@? "status"
        <*> x .@? "statusMessage"
        <*> x .@? "url"
        <*> x .@? "userBucket"

instance ToQuery SnapshotDetail where
    toQuery SnapshotDetail{..} = mconcat
        [ "Description"   =? _sdDescription
        , "DeviceName"    =? _sdDeviceName
        , "DiskImageSize" =? _sdDiskImageSize
        , "Format"        =? _sdFormat
        , "Progress"      =? _sdProgress
        , "SnapshotId"    =? _sdSnapshotId
        , "Status"        =? _sdStatus
        , "StatusMessage" =? _sdStatusMessage
        , "Url"           =? _sdUrl
        , "UserBucket"    =? _sdUserBucket
        ]

data PriceSchedule = PriceSchedule
    { _psActive       :: Maybe Bool
    , _psCurrencyCode :: Maybe CurrencyCodeValues
    , _psPrice        :: Maybe Double
    , _psTerm         :: Maybe Integer
    } deriving (Eq, Read, Show)

-- | 'PriceSchedule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psActive' @::@ 'Maybe' 'Bool'
--
-- * 'psCurrencyCode' @::@ 'Maybe' 'CurrencyCodeValues'
--
-- * 'psPrice' @::@ 'Maybe' 'Double'
--
-- * 'psTerm' @::@ 'Maybe' 'Integer'
--
priceSchedule :: PriceSchedule
priceSchedule = PriceSchedule
    { _psTerm         = Nothing
    , _psPrice        = Nothing
    , _psCurrencyCode = Nothing
    , _psActive       = Nothing
    }

-- | The current price schedule, as determined by the term remaining for the
-- Reserved Instance in the listing.
--
-- A specific price schedule is always in effect, but only one price schedule
-- can be active at any time. Take, for example, a Reserved Instance listing
-- that has five months remaining in its term. When you specify price schedules
-- for five months and two months, this means that schedule 1, covering the
-- first three months of the remaining term, will be active during months 5, 4,
-- and 3. Then schedule 2, covering the last two months of the term, will be
-- active for months 2 and 1.
psActive :: Lens' PriceSchedule (Maybe Bool)
psActive = lens _psActive (\s a -> s { _psActive = a })

-- | The currency for transacting the Reserved Instance resale. At this time, the
-- only supported currency is 'USD'.
psCurrencyCode :: Lens' PriceSchedule (Maybe CurrencyCodeValues)
psCurrencyCode = lens _psCurrencyCode (\s a -> s { _psCurrencyCode = a })

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice = lens _psPrice (\s a -> s { _psPrice = a })

-- | The number of months remaining in the reservation. For example, 2 is the
-- second to the last month before the capacity reservation expires.
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm = lens _psTerm (\s a -> s { _psTerm = a })

instance FromXML PriceSchedule where
    parseXML x = PriceSchedule
        <$> x .@? "active"
        <*> x .@? "currencyCode"
        <*> x .@? "price"
        <*> x .@? "term"

instance ToQuery PriceSchedule where
    toQuery PriceSchedule{..} = mconcat
        [ "Active"       =? _psActive
        , "CurrencyCode" =? _psCurrencyCode
        , "Price"        =? _psPrice
        , "Term"         =? _psTerm
        ]

data DeviceType
    = Ebs           -- ^ ebs
    | InstanceStore -- ^ instance-store
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DeviceType

instance FromText DeviceType where
    parser = takeLowerText >>= \case
        "ebs"            -> pure Ebs
        "instance-store" -> pure InstanceStore
        e                -> fail $
            "Failure parsing DeviceType from " ++ show e

instance ToText DeviceType where
    toText = \case
        Ebs           -> "ebs"
        InstanceStore -> "instance-store"

instance ToByteString DeviceType
instance ToHeader     DeviceType
instance ToQuery      DeviceType

instance FromXML DeviceType where
    parseXML = parseXMLText "DeviceType"

data DomainType
    = DTStandard -- ^ standard
    | DTVpc      -- ^ vpc
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DomainType

instance FromText DomainType where
    parser = takeLowerText >>= \case
        "standard" -> pure DTStandard
        "vpc"      -> pure DTVpc
        e          -> fail $
            "Failure parsing DomainType from " ++ show e

instance ToText DomainType where
    toText = \case
        DTStandard -> "standard"
        DTVpc      -> "vpc"

instance ToByteString DomainType
instance ToHeader     DomainType
instance ToQuery      DomainType

instance FromXML DomainType where
    parseXML = parseXMLText "DomainType"

data Region = Region
    { _rEndpoint   :: Maybe Text
    , _rRegionName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Region' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rEndpoint' @::@ 'Maybe' 'Text'
--
-- * 'rRegionName' @::@ 'Maybe' 'Text'
--
region :: Region
region = Region
    { _rRegionName = Nothing
    , _rEndpoint   = Nothing
    }

-- | The region service endpoint.
rEndpoint :: Lens' Region (Maybe Text)
rEndpoint = lens _rEndpoint (\s a -> s { _rEndpoint = a })

-- | The name of the region.
rRegionName :: Lens' Region (Maybe Text)
rRegionName = lens _rRegionName (\s a -> s { _rRegionName = a })

instance FromXML Region where
    parseXML x = Region
        <$> x .@? "regionEndpoint"
        <*> x .@? "regionName"

instance ToQuery Region where
    toQuery Region{..} = mconcat
        [ "RegionEndpoint" =? _rEndpoint
        , "RegionName"     =? _rRegionName
        ]

newtype PropagatingVgw = PropagatingVgw
    { _pvGatewayId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'PropagatingVgw' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pvGatewayId' @::@ 'Maybe' 'Text'
--
propagatingVgw :: PropagatingVgw
propagatingVgw = PropagatingVgw
    { _pvGatewayId = Nothing
    }

-- | The ID of the virtual private gateway (VGW).
pvGatewayId :: Lens' PropagatingVgw (Maybe Text)
pvGatewayId = lens _pvGatewayId (\s a -> s { _pvGatewayId = a })

instance FromXML PropagatingVgw where
    parseXML x = PropagatingVgw
        <$> x .@? "gatewayId"

instance ToQuery PropagatingVgw where
    toQuery PropagatingVgw{..} = mconcat
        [ "GatewayId" =? _pvGatewayId
        ]

data OfferingTypeValues
    = AllUpfront        -- ^ All Upfront
    | HeavyUtilization  -- ^ Heavy Utilization
    | LightUtilization  -- ^ Light Utilization
    | MediumUtilization -- ^ Medium Utilization
    | NoUpfront         -- ^ No Upfront
    | PartialUpfront    -- ^ Partial Upfront
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable OfferingTypeValues

instance FromText OfferingTypeValues where
    parser = takeLowerText >>= \case
        "all upfront"        -> pure AllUpfront
        "heavy utilization"  -> pure HeavyUtilization
        "light utilization"  -> pure LightUtilization
        "medium utilization" -> pure MediumUtilization
        "no upfront"         -> pure NoUpfront
        "partial upfront"    -> pure PartialUpfront
        e                    -> fail $
            "Failure parsing OfferingTypeValues from " ++ show e

instance ToText OfferingTypeValues where
    toText = \case
        AllUpfront        -> "All Upfront"
        HeavyUtilization  -> "Heavy Utilization"
        LightUtilization  -> "Light Utilization"
        MediumUtilization -> "Medium Utilization"
        NoUpfront         -> "No Upfront"
        PartialUpfront    -> "Partial Upfront"

instance ToByteString OfferingTypeValues
instance ToHeader     OfferingTypeValues
instance ToQuery      OfferingTypeValues

instance FromXML OfferingTypeValues where
    parseXML = parseXMLText "OfferingTypeValues"

data VpnGateway = VpnGateway
    { _vgAvailabilityZone :: Maybe Text
    , _vgState            :: Maybe VpnState
    , _vgTags             :: List "item" Tag
    , _vgType             :: Maybe GatewayType
    , _vgVpcAttachments   :: List "item" VpcAttachment
    , _vgVpnGatewayId     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VpnGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vgAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'vgState' @::@ 'Maybe' 'VpnState'
--
-- * 'vgTags' @::@ ['Tag']
--
-- * 'vgType' @::@ 'Maybe' 'GatewayType'
--
-- * 'vgVpcAttachments' @::@ ['VpcAttachment']
--
-- * 'vgVpnGatewayId' @::@ 'Maybe' 'Text'
--
vpnGateway :: VpnGateway
vpnGateway = VpnGateway
    { _vgVpnGatewayId     = Nothing
    , _vgState            = Nothing
    , _vgType             = Nothing
    , _vgAvailabilityZone = Nothing
    , _vgVpcAttachments   = mempty
    , _vgTags             = mempty
    }

-- | The Availability Zone where the virtual private gateway was created.
vgAvailabilityZone :: Lens' VpnGateway (Maybe Text)
vgAvailabilityZone =
    lens _vgAvailabilityZone (\s a -> s { _vgAvailabilityZone = a })

-- | The current state of the virtual private gateway.
vgState :: Lens' VpnGateway (Maybe VpnState)
vgState = lens _vgState (\s a -> s { _vgState = a })

-- | Any tags assigned to the virtual private gateway.
vgTags :: Lens' VpnGateway [Tag]
vgTags = lens _vgTags (\s a -> s { _vgTags = a }) . _List

-- | The type of VPN connection the virtual private gateway supports.
vgType :: Lens' VpnGateway (Maybe GatewayType)
vgType = lens _vgType (\s a -> s { _vgType = a })

-- | Any VPCs attached to the virtual private gateway.
vgVpcAttachments :: Lens' VpnGateway [VpcAttachment]
vgVpcAttachments = lens _vgVpcAttachments (\s a -> s { _vgVpcAttachments = a }) . _List

-- | The ID of the virtual private gateway.
vgVpnGatewayId :: Lens' VpnGateway (Maybe Text)
vgVpnGatewayId = lens _vgVpnGatewayId (\s a -> s { _vgVpnGatewayId = a })

instance FromXML VpnGateway where
    parseXML x = VpnGateway
        <$> x .@? "availabilityZone"
        <*> x .@? "state"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "type"
        <*> x .@? "attachments" .!@ mempty
        <*> x .@? "vpnGatewayId"

instance ToQuery VpnGateway where
    toQuery VpnGateway{..} = mconcat
        [ "AvailabilityZone" =? _vgAvailabilityZone
        , "State"            =? _vgState
        , "TagSet"           `toQueryList` _vgTags
        , "Type"             =? _vgType
        , "Attachments"      `toQueryList` _vgVpcAttachments
        , "VpnGatewayId"     =? _vgVpnGatewayId
        ]

data EventInformation = EventInformation
    { _eiEventDescription :: Maybe Text
    , _eiEventSubType     :: Maybe Text
    , _eiInstanceId       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'EventInformation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiEventDescription' @::@ 'Maybe' 'Text'
--
-- * 'eiEventSubType' @::@ 'Maybe' 'Text'
--
-- * 'eiInstanceId' @::@ 'Maybe' 'Text'
--
eventInformation :: EventInformation
eventInformation = EventInformation
    { _eiInstanceId       = Nothing
    , _eiEventSubType     = Nothing
    , _eiEventDescription = Nothing
    }

-- | The description of the event.
eiEventDescription :: Lens' EventInformation (Maybe Text)
eiEventDescription =
    lens _eiEventDescription (\s a -> s { _eiEventDescription = a })

-- | The event.
--
-- The following are the 'error' events.
--
-- 'iamFleetRoleInvalid' - Spot fleet did not have the required permissions
-- either to launch or terminate an instance.
--
-- 'spotFleetRequestConfigurationInvalid' - The configuration is not valid. For
-- more information, see the description.   'spotInstanceCountLimitExceeded' -
-- You've reached the limit on the number of Spot Instances that you can launch.
--
-- The following are the 'fleetRequestChange' events.
--
-- 'active' - The Spot fleet has been validated and Amazon EC2 is attempting to
-- maintain the target number of running Spot Instances.
--
-- 'cancelled' - The Spot fleet is canceled and has no running Spot Instances.
-- The Spot fleet will be deleted two days after its instances were terminated.
--
-- 'cancelled_running' - The Spot fleet is canceled and will not launch
-- additional Spot Instances, but its existing Spot Instances will continue to
-- run until they are interrupted or terminated.
--
-- 'cancelled_terminating' - The Spot fleet is canceled and its Spot Instances
-- are terminating.
--
-- 'expired' - The Spot fleet request has expired. A subsequent event indicates
-- that the instances were terminated, if the request was created with 'terminateInstancesWithExpiration' set.
--
-- 'price_update' - The bid price for a launch configuration was adjusted
-- because it was too high. This change is permanent.
--
-- 'submitted' - The Spot fleet request is being evaluated and Amazon EC2 is
-- preparing to launch the target number of Spot Instances.
--
-- The following are the 'instanceChange' events.
--
-- 'launched' - A bid was fulfilled and a new instance was launched.
--
-- 'terminated' - An instance was terminated by the user.
--
--
eiEventSubType :: Lens' EventInformation (Maybe Text)
eiEventSubType = lens _eiEventSubType (\s a -> s { _eiEventSubType = a })

-- | The ID of the instance. This information is available only for 'instanceChange'
-- events.
eiInstanceId :: Lens' EventInformation (Maybe Text)
eiInstanceId = lens _eiInstanceId (\s a -> s { _eiInstanceId = a })

instance FromXML EventInformation where
    parseXML x = EventInformation
        <$> x .@? "eventDescription"
        <*> x .@? "eventSubType"
        <*> x .@? "instanceId"

instance ToQuery EventInformation where
    toQuery EventInformation{..} = mconcat
        [ "EventDescription" =? _eiEventDescription
        , "EventSubType"     =? _eiEventSubType
        , "InstanceId"       =? _eiInstanceId
        ]

data Filter = Filter
    { _fName   :: Text
    , _fValues :: List "item" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Filter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fName' @::@ 'Text'
--
-- * 'fValues' @::@ ['Text']
--
filter' :: Text -- ^ 'fName'
        -> Filter
filter' p1 = Filter
    { _fName   = p1
    , _fValues = mempty
    }

-- | The name of the filter. Filter names are case-sensitive.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s { _fName = a })

-- | One or more filter values. Filter values are case-sensitive.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s { _fValues = a }) . _List

instance FromXML Filter where
    parseXML x = Filter
        <$> x .@  "Name"
        <*> x .@? "Value" .!@ mempty

instance ToQuery Filter where
    toQuery Filter{..} = mconcat
        [ "Name"  =? _fName
        , "Value" `toQueryList` _fValues
        ]

data VolumeType
    = Gp2      -- ^ gp2
    | Io1      -- ^ io1
    | Standard -- ^ standard
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VolumeType

instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2"      -> pure Gp2
        "io1"      -> pure Io1
        "standard" -> pure Standard
        e          -> fail $
            "Failure parsing VolumeType from " ++ show e

instance ToText VolumeType where
    toText = \case
        Gp2      -> "gp2"
        Io1      -> "io1"
        Standard -> "standard"

instance ToByteString VolumeType
instance ToHeader     VolumeType
instance ToQuery      VolumeType

instance FromXML VolumeType where
    parseXML = parseXMLText "VolumeType"

data InstanceStateChange = InstanceStateChange
    { _iscCurrentState  :: Maybe InstanceState
    , _iscInstanceId    :: Maybe Text
    , _iscPreviousState :: Maybe InstanceState
    } deriving (Eq, Read, Show)

-- | 'InstanceStateChange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iscCurrentState' @::@ 'Maybe' 'InstanceState'
--
-- * 'iscInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iscPreviousState' @::@ 'Maybe' 'InstanceState'
--
instanceStateChange :: InstanceStateChange
instanceStateChange = InstanceStateChange
    { _iscInstanceId    = Nothing
    , _iscCurrentState  = Nothing
    , _iscPreviousState = Nothing
    }

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState = lens _iscCurrentState (\s a -> s { _iscCurrentState = a })

-- | The ID of the instance.
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId = lens _iscInstanceId (\s a -> s { _iscInstanceId = a })

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState = lens _iscPreviousState (\s a -> s { _iscPreviousState = a })

instance FromXML InstanceStateChange where
    parseXML x = InstanceStateChange
        <$> x .@? "currentState"
        <*> x .@? "instanceId"
        <*> x .@? "previousState"

instance ToQuery InstanceStateChange where
    toQuery InstanceStateChange{..} = mconcat
        [ "CurrentState"  =? _iscCurrentState
        , "InstanceId"    =? _iscInstanceId
        , "PreviousState" =? _iscPreviousState
        ]

data NetworkAcl = NetworkAcl
    { _naAssociations :: List "item" NetworkAclAssociation
    , _naEntries      :: List "item" NetworkAclEntry
    , _naIsDefault    :: Maybe Bool
    , _naNetworkAclId :: Maybe Text
    , _naTags         :: List "item" Tag
    , _naVpcId        :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'NetworkAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'naAssociations' @::@ ['NetworkAclAssociation']
--
-- * 'naEntries' @::@ ['NetworkAclEntry']
--
-- * 'naIsDefault' @::@ 'Maybe' 'Bool'
--
-- * 'naNetworkAclId' @::@ 'Maybe' 'Text'
--
-- * 'naTags' @::@ ['Tag']
--
-- * 'naVpcId' @::@ 'Maybe' 'Text'
--
networkAcl :: NetworkAcl
networkAcl = NetworkAcl
    { _naNetworkAclId = Nothing
    , _naVpcId        = Nothing
    , _naIsDefault    = Nothing
    , _naEntries      = mempty
    , _naAssociations = mempty
    , _naTags         = mempty
    }

-- | Any associations between the network ACL and one or more subnets
naAssociations :: Lens' NetworkAcl [NetworkAclAssociation]
naAssociations = lens _naAssociations (\s a -> s { _naAssociations = a }) . _List

-- | One or more entries (rules) in the network ACL.
naEntries :: Lens' NetworkAcl [NetworkAclEntry]
naEntries = lens _naEntries (\s a -> s { _naEntries = a }) . _List

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault :: Lens' NetworkAcl (Maybe Bool)
naIsDefault = lens _naIsDefault (\s a -> s { _naIsDefault = a })

-- | The ID of the network ACL.
naNetworkAclId :: Lens' NetworkAcl (Maybe Text)
naNetworkAclId = lens _naNetworkAclId (\s a -> s { _naNetworkAclId = a })

-- | Any tags assigned to the network ACL.
naTags :: Lens' NetworkAcl [Tag]
naTags = lens _naTags (\s a -> s { _naTags = a }) . _List

-- | The ID of the VPC for the network ACL.
naVpcId :: Lens' NetworkAcl (Maybe Text)
naVpcId = lens _naVpcId (\s a -> s { _naVpcId = a })

instance FromXML NetworkAcl where
    parseXML x = NetworkAcl
        <$> x .@? "associationSet" .!@ mempty
        <*> x .@? "entrySet" .!@ mempty
        <*> x .@? "default"
        <*> x .@? "networkAclId"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "vpcId"

instance ToQuery NetworkAcl where
    toQuery NetworkAcl{..} = mconcat
        [ "AssociationSet" `toQueryList` _naAssociations
        , "EntrySet"       `toQueryList` _naEntries
        , "Default"        =? _naIsDefault
        , "NetworkAclId"   =? _naNetworkAclId
        , "TagSet"         `toQueryList` _naTags
        , "VpcId"          =? _naVpcId
        ]

data ImageState
    = ISAvailable    -- ^ available
    | ISDeregistered -- ^ deregistered
    | ISError        -- ^ error
    | ISFailed       -- ^ failed
    | ISInvalid      -- ^ invalid
    | ISPending      -- ^ pending
    | ISTransient    -- ^ transient
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ImageState

instance FromText ImageState where
    parser = takeLowerText >>= \case
        "available"    -> pure ISAvailable
        "deregistered" -> pure ISDeregistered
        "error"        -> pure ISError
        "failed"       -> pure ISFailed
        "invalid"      -> pure ISInvalid
        "pending"      -> pure ISPending
        "transient"    -> pure ISTransient
        e              -> fail $
            "Failure parsing ImageState from " ++ show e

instance ToText ImageState where
    toText = \case
        ISAvailable    -> "available"
        ISDeregistered -> "deregistered"
        ISError        -> "error"
        ISFailed       -> "failed"
        ISInvalid      -> "invalid"
        ISPending      -> "pending"
        ISTransient    -> "transient"

instance ToByteString ImageState
instance ToHeader     ImageState
instance ToQuery      ImageState

instance FromXML ImageState where
    parseXML = parseXMLText "ImageState"

data GatewayType
    = Ipsec1 -- ^ ipsec.1
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable GatewayType

instance FromText GatewayType where
    parser = takeLowerText >>= \case
        "ipsec.1" -> pure Ipsec1
        e         -> fail $
            "Failure parsing GatewayType from " ++ show e

instance ToText GatewayType where
    toText Ipsec1 = "ipsec.1"

instance ToByteString GatewayType
instance ToHeader     GatewayType
instance ToQuery      GatewayType

instance FromXML GatewayType where
    parseXML = parseXMLText "GatewayType"

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachTime          :: Maybe ISO8601
    , _iniaAttachmentId        :: Maybe Text
    , _iniaDeleteOnTermination :: Maybe Bool
    , _iniaDeviceIndex         :: Maybe Int
    , _iniaStatus              :: Maybe AttachmentStatus
    } deriving (Eq, Read, Show)

-- | 'InstanceNetworkInterfaceAttachment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iniaAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'iniaAttachmentId' @::@ 'Maybe' 'Text'
--
-- * 'iniaDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'iniaDeviceIndex' @::@ 'Maybe' 'Int'
--
-- * 'iniaStatus' @::@ 'Maybe' 'AttachmentStatus'
--
instanceNetworkInterfaceAttachment :: InstanceNetworkInterfaceAttachment
instanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { _iniaAttachmentId        = Nothing
    , _iniaDeviceIndex         = Nothing
    , _iniaStatus              = Nothing
    , _iniaAttachTime          = Nothing
    , _iniaDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe UTCTime)
iniaAttachTime = lens _iniaAttachTime (\s a -> s { _iniaAttachTime = a }) . mapping _Time

-- | The ID of the network interface attachment.
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId = lens _iniaAttachmentId (\s a -> s { _iniaAttachmentId = a })

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination =
    lens _iniaDeleteOnTermination (\s a -> s { _iniaDeleteOnTermination = a })

-- | The index of the device on the instance for the network interface attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Int)
iniaDeviceIndex = lens _iniaDeviceIndex (\s a -> s { _iniaDeviceIndex = a })

-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe AttachmentStatus)
iniaStatus = lens _iniaStatus (\s a -> s { _iniaStatus = a })

instance FromXML InstanceNetworkInterfaceAttachment where
    parseXML x = InstanceNetworkInterfaceAttachment
        <$> x .@? "attachTime"
        <*> x .@? "attachmentId"
        <*> x .@? "deleteOnTermination"
        <*> x .@? "deviceIndex"
        <*> x .@? "status"

instance ToQuery InstanceNetworkInterfaceAttachment where
    toQuery InstanceNetworkInterfaceAttachment{..} = mconcat
        [ "AttachTime"          =? _iniaAttachTime
        , "AttachmentId"        =? _iniaAttachmentId
        , "DeleteOnTermination" =? _iniaDeleteOnTermination
        , "DeviceIndex"         =? _iniaDeviceIndex
        , "Status"              =? _iniaStatus
        ]

newtype AttributeBooleanValue = AttributeBooleanValue
    { _abvValue :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'AttributeBooleanValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'abvValue' @::@ 'Maybe' 'Bool'
--
attributeBooleanValue :: AttributeBooleanValue
attributeBooleanValue = AttributeBooleanValue
    { _abvValue = Nothing
    }

-- | Valid values are 'true' or 'false'.
abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue = lens _abvValue (\s a -> s { _abvValue = a })

instance FromXML AttributeBooleanValue where
    parseXML x = AttributeBooleanValue
        <$> x .@? "value"

instance ToQuery AttributeBooleanValue where
    toQuery AttributeBooleanValue{..} = mconcat
        [ "Value" =? _abvValue
        ]

data RecurringCharge = RecurringCharge
    { _rcAmount    :: Maybe Double
    , _rcFrequency :: Maybe RecurringChargeFrequency
    } deriving (Eq, Read, Show)

-- | 'RecurringCharge' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcAmount' @::@ 'Maybe' 'Double'
--
-- * 'rcFrequency' @::@ 'Maybe' 'RecurringChargeFrequency'
--
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge
    { _rcFrequency = Nothing
    , _rcAmount    = Nothing
    }

-- | The amount of the recurring charge.
rcAmount :: Lens' RecurringCharge (Maybe Double)
rcAmount = lens _rcAmount (\s a -> s { _rcAmount = a })

-- | The frequency of the recurring charge.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\s a -> s { _rcFrequency = a })

instance FromXML RecurringCharge where
    parseXML x = RecurringCharge
        <$> x .@? "amount"
        <*> x .@? "frequency"

instance ToQuery RecurringCharge where
    toQuery RecurringCharge{..} = mconcat
        [ "Amount"    =? _rcAmount
        , "Frequency" =? _rcFrequency
        ]

data NewDhcpConfiguration = NewDhcpConfiguration
    { _ndcKey    :: Maybe Text
    , _ndcValues :: List "item" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'NewDhcpConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ndcKey' @::@ 'Maybe' 'Text'
--
-- * 'ndcValues' @::@ ['Text']
--
newDhcpConfiguration :: NewDhcpConfiguration
newDhcpConfiguration = NewDhcpConfiguration
    { _ndcKey    = Nothing
    , _ndcValues = mempty
    }

ndcKey :: Lens' NewDhcpConfiguration (Maybe Text)
ndcKey = lens _ndcKey (\s a -> s { _ndcKey = a })

ndcValues :: Lens' NewDhcpConfiguration [Text]
ndcValues = lens _ndcValues (\s a -> s { _ndcValues = a }) . _List

instance FromXML NewDhcpConfiguration where
    parseXML x = NewDhcpConfiguration
        <$> x .@? "key"
        <*> x .@? "Value" .!@ mempty

instance ToQuery NewDhcpConfiguration where
    toQuery NewDhcpConfiguration{..} = mconcat
        [ "Key"   =? _ndcKey
        , "Value" `toQueryList` _ndcValues
        ]

data StateReason = StateReason
    { _srCode    :: Maybe Text
    , _srMessage :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'StateReason' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srCode' @::@ 'Maybe' 'Text'
--
-- * 'srMessage' @::@ 'Maybe' 'Text'
--
stateReason :: StateReason
stateReason = StateReason
    { _srCode    = Nothing
    , _srMessage = Nothing
    }

-- | The reason code for the state change.
srCode :: Lens' StateReason (Maybe Text)
srCode = lens _srCode (\s a -> s { _srCode = a })

-- | The message for the state change.
--
-- 'Server.SpotInstanceTermination': A Spot Instance was terminated due to an
-- increase in the market price.
--
-- 'Server.InternalError': An internal error occurred during instance launch,
-- resulting in termination.
--
-- 'Server.InsufficientInstanceCapacity': There was insufficient instance
-- capacity to satisfy the launch request.
--
-- 'Client.InternalError': A client error caused the instance to terminate on
-- launch.
--
-- 'Client.InstanceInitiatedShutdown': The instance was shut down using the 'shutdown -h' command from the instance.
--
-- 'Client.UserInitiatedShutdown': The instance was shut down using the Amazon
-- EC2 API.
--
-- 'Client.VolumeLimitExceeded': The volume limit was exceeded.
--
-- 'Client.InvalidSnapshot.NotFound': The specified snapshot was not found.
--
--
srMessage :: Lens' StateReason (Maybe Text)
srMessage = lens _srMessage (\s a -> s { _srMessage = a })

instance FromXML StateReason where
    parseXML x = StateReason
        <$> x .@? "code"
        <*> x .@? "message"

instance ToQuery StateReason where
    toQuery StateReason{..} = mconcat
        [ "Code"    =? _srCode
        , "Message" =? _srMessage
        ]

data MonitoringState
    = MSDisabled  -- ^ disabled
    | MSDisabling -- ^ disabling
    | MSEnabled   -- ^ enabled
    | MSPending   -- ^ pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable MonitoringState

instance FromText MonitoringState where
    parser = takeLowerText >>= \case
        "disabled"  -> pure MSDisabled
        "disabling" -> pure MSDisabling
        "enabled"   -> pure MSEnabled
        "pending"   -> pure MSPending
        e           -> fail $
            "Failure parsing MonitoringState from " ++ show e

instance ToText MonitoringState where
    toText = \case
        MSDisabled  -> "disabled"
        MSDisabling -> "disabling"
        MSEnabled   -> "enabled"
        MSPending   -> "pending"

instance ToByteString MonitoringState
instance ToHeader     MonitoringState
instance ToQuery      MonitoringState

instance FromXML MonitoringState where
    parseXML = parseXMLText "MonitoringState"

newtype ReservedInstancesId = ReservedInstancesId
    { _riiReservedInstancesId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ReservedInstancesId' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riiReservedInstancesId' @::@ 'Maybe' 'Text'
--
reservedInstancesId :: ReservedInstancesId
reservedInstancesId = ReservedInstancesId
    { _riiReservedInstancesId = Nothing
    }

-- | The ID of the Reserved Instance.
riiReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
riiReservedInstancesId =
    lens _riiReservedInstancesId (\s a -> s { _riiReservedInstancesId = a })

instance FromXML ReservedInstancesId where
    parseXML x = ReservedInstancesId
        <$> x .@? "reservedInstancesId"

instance ToQuery ReservedInstancesId where
    toQuery ReservedInstancesId{..} = mconcat
        [ "ReservedInstancesId" =? _riiReservedInstancesId
        ]

data StatusName
    = Reachability -- ^ reachability
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable StatusName

instance FromText StatusName where
    parser = takeLowerText >>= \case
        "reachability" -> pure Reachability
        e              -> fail $
            "Failure parsing StatusName from " ++ show e

instance ToText StatusName where
    toText Reachability = "reachability"

instance ToByteString StatusName
instance ToHeader     StatusName
instance ToQuery      StatusName

instance FromXML StatusName where
    parseXML = parseXMLText "StatusName"

data InternetGateway = InternetGateway
    { _igAttachments       :: List "item" InternetGatewayAttachment
    , _igInternetGatewayId :: Text
    , _igTags              :: List "item" Tag
    } deriving (Eq, Read, Show)

-- | 'InternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'igAttachments' @::@ ['InternetGatewayAttachment']
--
-- * 'igInternetGatewayId' @::@ 'Text'
--
-- * 'igTags' @::@ ['Tag']
--
internetGateway :: Text -- ^ 'igInternetGatewayId'
                -> InternetGateway
internetGateway p1 = InternetGateway
    { _igInternetGatewayId = p1
    , _igAttachments       = mempty
    , _igTags              = mempty
    }

-- | Any VPCs attached to the Internet gateway.
igAttachments :: Lens' InternetGateway [InternetGatewayAttachment]
igAttachments = lens _igAttachments (\s a -> s { _igAttachments = a }) . _List

-- | The ID of the Internet gateway.
igInternetGatewayId :: Lens' InternetGateway Text
igInternetGatewayId =
    lens _igInternetGatewayId (\s a -> s { _igInternetGatewayId = a })

-- | Any tags assigned to the Internet gateway.
igTags :: Lens' InternetGateway [Tag]
igTags = lens _igTags (\s a -> s { _igTags = a }) . _List

instance FromXML InternetGateway where
    parseXML x = InternetGateway
        <$> x .@? "attachmentSet" .!@ mempty
        <*> x .@  "internetGatewayId"
        <*> x .@? "tagSet" .!@ mempty

instance ToQuery InternetGateway where
    toQuery InternetGateway{..} = mconcat
        [ "AttachmentSet"     `toQueryList` _igAttachments
        , "InternetGatewayId" =? _igInternetGatewayId
        , "TagSet"            `toQueryList` _igTags
        ]

data VolumeStatusName
    = IoEnabled     -- ^ io-enabled
    | IoPerformance -- ^ io-performance
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VolumeStatusName

instance FromText VolumeStatusName where
    parser = takeLowerText >>= \case
        "io-enabled"     -> pure IoEnabled
        "io-performance" -> pure IoPerformance
        e                -> fail $
            "Failure parsing VolumeStatusName from " ++ show e

instance ToText VolumeStatusName where
    toText = \case
        IoEnabled     -> "io-enabled"
        IoPerformance -> "io-performance"

instance ToByteString VolumeStatusName
instance ToHeader     VolumeStatusName
instance ToQuery      VolumeStatusName

instance FromXML VolumeStatusName where
    parseXML = parseXMLText "VolumeStatusName"

data VolumeAttributeName
    = AutoEnableIO -- ^ autoEnableIO
    | ProductCodes -- ^ productCodes
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VolumeAttributeName

instance FromText VolumeAttributeName where
    parser = takeLowerText >>= \case
        "autoenableio" -> pure AutoEnableIO
        "productcodes" -> pure ProductCodes
        e              -> fail $
            "Failure parsing VolumeAttributeName from " ++ show e

instance ToText VolumeAttributeName where
    toText = \case
        AutoEnableIO -> "autoEnableIO"
        ProductCodes -> "productCodes"

instance ToByteString VolumeAttributeName
instance ToHeader     VolumeAttributeName
instance ToQuery      VolumeAttributeName

instance FromXML VolumeAttributeName where
    parseXML = parseXMLText "VolumeAttributeName"

data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdDescription :: Maybe Text
    , _iitdInstanceId  :: Maybe Text
    , _iitdPlatform    :: Maybe PlatformValues
    , _iitdVolumes     :: List "item" ImportInstanceVolumeDetailItem
    } deriving (Eq, Read, Show)

-- | 'ImportInstanceTaskDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iitdDescription' @::@ 'Maybe' 'Text'
--
-- * 'iitdInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'iitdPlatform' @::@ 'Maybe' 'PlatformValues'
--
-- * 'iitdVolumes' @::@ ['ImportInstanceVolumeDetailItem']
--
importInstanceTaskDetails :: ImportInstanceTaskDetails
importInstanceTaskDetails = ImportInstanceTaskDetails
    { _iitdVolumes     = mempty
    , _iitdInstanceId  = Nothing
    , _iitdPlatform    = Nothing
    , _iitdDescription = Nothing
    }

-- | A description of the task.
iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription = lens _iitdDescription (\s a -> s { _iitdDescription = a })

-- | The ID of the instance.
iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId = lens _iitdInstanceId (\s a -> s { _iitdInstanceId = a })

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe PlatformValues)
iitdPlatform = lens _iitdPlatform (\s a -> s { _iitdPlatform = a })

-- | One or more volumes.
iitdVolumes :: Lens' ImportInstanceTaskDetails [ImportInstanceVolumeDetailItem]
iitdVolumes = lens _iitdVolumes (\s a -> s { _iitdVolumes = a }) . _List

instance FromXML ImportInstanceTaskDetails where
    parseXML x = ImportInstanceTaskDetails
        <$> x .@? "description"
        <*> x .@? "instanceId"
        <*> x .@? "platform"
        <*> x .@? "volumes" .!@ mempty

instance ToQuery ImportInstanceTaskDetails where
    toQuery ImportInstanceTaskDetails{..} = mconcat
        [ "Description" =? _iitdDescription
        , "InstanceId"  =? _iitdInstanceId
        , "Platform"    =? _iitdPlatform
        , "Volumes"     `toQueryList` _iitdVolumes
        ]

data PlacementGroup = PlacementGroup
    { _pgGroupName :: Maybe Text
    , _pgState     :: Maybe PlacementGroupState
    , _pgStrategy  :: Maybe PlacementStrategy
    } deriving (Eq, Read, Show)

-- | 'PlacementGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgGroupName' @::@ 'Maybe' 'Text'
--
-- * 'pgState' @::@ 'Maybe' 'PlacementGroupState'
--
-- * 'pgStrategy' @::@ 'Maybe' 'PlacementStrategy'
--
placementGroup :: PlacementGroup
placementGroup = PlacementGroup
    { _pgGroupName = Nothing
    , _pgStrategy  = Nothing
    , _pgState     = Nothing
    }

-- | The name of the placement group.
pgGroupName :: Lens' PlacementGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\s a -> s { _pgGroupName = a })

-- | The state of the placement group.
pgState :: Lens' PlacementGroup (Maybe PlacementGroupState)
pgState = lens _pgState (\s a -> s { _pgState = a })

-- | The placement strategy.
pgStrategy :: Lens' PlacementGroup (Maybe PlacementStrategy)
pgStrategy = lens _pgStrategy (\s a -> s { _pgStrategy = a })

instance FromXML PlacementGroup where
    parseXML x = PlacementGroup
        <$> x .@? "groupName"
        <*> x .@? "state"
        <*> x .@? "strategy"

instance ToQuery PlacementGroup where
    toQuery PlacementGroup{..} = mconcat
        [ "GroupName" =? _pgGroupName
        , "State"     =? _pgState
        , "Strategy"  =? _pgStrategy
        ]

data ProductCode = ProductCode
    { _pcProductCodeId   :: Maybe Text
    , _pcProductCodeType :: Maybe ProductCodeValues
    } deriving (Eq, Read, Show)

-- | 'ProductCode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pcProductCodeId' @::@ 'Maybe' 'Text'
--
-- * 'pcProductCodeType' @::@ 'Maybe' 'ProductCodeValues'
--
productCode :: ProductCode
productCode = ProductCode
    { _pcProductCodeId   = Nothing
    , _pcProductCodeType = Nothing
    }

-- | The product code.
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId = lens _pcProductCodeId (\s a -> s { _pcProductCodeId = a })

-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe ProductCodeValues)
pcProductCodeType =
    lens _pcProductCodeType (\s a -> s { _pcProductCodeType = a })

instance FromXML ProductCode where
    parseXML x = ProductCode
        <$> x .@? "productCode"
        <*> x .@? "type"

instance ToQuery ProductCode where
    toQuery ProductCode{..} = mconcat
        [ "ProductCode" =? _pcProductCodeId
        , "Type"        =? _pcProductCodeType
        ]

data ListingStatus
    = ListingStatusActive    -- ^ active
    | ListingStatusCancelled -- ^ cancelled
    | ListingStatusClosed    -- ^ closed
    | ListingStatusPending   -- ^ pending
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ListingStatus

instance FromText ListingStatus where
    parser = takeLowerText >>= \case
        "active"    -> pure ListingStatusActive
        "cancelled" -> pure ListingStatusCancelled
        "closed"    -> pure ListingStatusClosed
        "pending"   -> pure ListingStatusPending
        e           -> fail $
            "Failure parsing ListingStatus from " ++ show e

instance ToText ListingStatus where
    toText = \case
        ListingStatusActive    -> "active"
        ListingStatusCancelled -> "cancelled"
        ListingStatusClosed    -> "closed"
        ListingStatusPending   -> "pending"

instance ToByteString ListingStatus
instance ToHeader     ListingStatus
instance ToQuery      ListingStatus

instance FromXML ListingStatus where
    parseXML = parseXMLText "ListingStatus"

newtype IpRange = IpRange
    { _irCidrIp :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'IpRange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irCidrIp' @::@ 'Text'
--
ipRange :: Text -- ^ 'irCidrIp'
        -> IpRange
ipRange p1 = IpRange
    { _irCidrIp = p1
    }

-- | The CIDR range. You can either specify a CIDR range or a source security
-- group, not both.
irCidrIp :: Lens' IpRange Text
irCidrIp = lens _irCidrIp (\s a -> s { _irCidrIp = a })

instance FromXML IpRange where
    parseXML x = IpRange
        <$> x .@  "cidrIp"

instance ToQuery IpRange where
    toQuery IpRange{..} = mconcat
        [ "CidrIp" =? _irCidrIp
        ]

data VolumeStatusInfoStatus
    = VSISImpaired         -- ^ impaired
    | VSISInsufficientData -- ^ insufficient-data
    | VSISOk               -- ^ ok
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable VolumeStatusInfoStatus

instance FromText VolumeStatusInfoStatus where
    parser = takeLowerText >>= \case
        "impaired"          -> pure VSISImpaired
        "insufficient-data" -> pure VSISInsufficientData
        "ok"                -> pure VSISOk
        e                   -> fail $
            "Failure parsing VolumeStatusInfoStatus from " ++ show e

instance ToText VolumeStatusInfoStatus where
    toText = \case
        VSISImpaired         -> "impaired"
        VSISInsufficientData -> "insufficient-data"
        VSISOk               -> "ok"

instance ToByteString VolumeStatusInfoStatus
instance ToHeader     VolumeStatusInfoStatus
instance ToQuery      VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    parseXML = parseXMLText "VolumeStatusInfoStatus"

newtype AccountAttributeValue = AccountAttributeValue
    { _aavAttributeValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AccountAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aavAttributeValue' @::@ 'Maybe' 'Text'
--
accountAttributeValue :: AccountAttributeValue
accountAttributeValue = AccountAttributeValue
    { _aavAttributeValue = Nothing
    }

-- | The value of the attribute.
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue =
    lens _aavAttributeValue (\s a -> s { _aavAttributeValue = a })

instance FromXML AccountAttributeValue where
    parseXML x = AccountAttributeValue
        <$> x .@? "attributeValue"

instance ToQuery AccountAttributeValue where
    toQuery AccountAttributeValue{..} = mconcat
        [ "AttributeValue" =? _aavAttributeValue
        ]

data SnapshotDiskContainer = SnapshotDiskContainer
    { _sdcDescription :: Maybe Text
    , _sdcFormat      :: Maybe Text
    , _sdcUrl         :: Maybe Text
    , _sdcUserBucket  :: Maybe UserBucket
    } deriving (Eq, Read, Show)

-- | 'SnapshotDiskContainer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdcDescription' @::@ 'Maybe' 'Text'
--
-- * 'sdcFormat' @::@ 'Maybe' 'Text'
--
-- * 'sdcUrl' @::@ 'Maybe' 'Text'
--
-- * 'sdcUserBucket' @::@ 'Maybe' 'UserBucket'
--
snapshotDiskContainer :: SnapshotDiskContainer
snapshotDiskContainer = SnapshotDiskContainer
    { _sdcDescription = Nothing
    , _sdcFormat      = Nothing
    , _sdcUrl         = Nothing
    , _sdcUserBucket  = Nothing
    }

-- | The description of the disk image being imported.
sdcDescription :: Lens' SnapshotDiskContainer (Maybe Text)
sdcDescription = lens _sdcDescription (\s a -> s { _sdcDescription = a })

-- | The format of the disk image being imported.
--
-- Valid values: 'RAW' | 'VHD' | 'VMDK' | 'OVA'
sdcFormat :: Lens' SnapshotDiskContainer (Maybe Text)
sdcFormat = lens _sdcFormat (\s a -> s { _sdcFormat = a })

-- | The URL to the Amazon S3-based disk image being imported. It can either be a
-- https URL (https://..) or an Amazon S3 URL (s3://..).
sdcUrl :: Lens' SnapshotDiskContainer (Maybe Text)
sdcUrl = lens _sdcUrl (\s a -> s { _sdcUrl = a })

sdcUserBucket :: Lens' SnapshotDiskContainer (Maybe UserBucket)
sdcUserBucket = lens _sdcUserBucket (\s a -> s { _sdcUserBucket = a })

instance FromXML SnapshotDiskContainer where
    parseXML x = SnapshotDiskContainer
        <$> x .@? "Description"
        <*> x .@? "Format"
        <*> x .@? "Url"
        <*> x .@? "UserBucket"

instance ToQuery SnapshotDiskContainer where
    toQuery SnapshotDiskContainer{..} = mconcat
        [ "Description" =? _sdcDescription
        , "Format"      =? _sdcFormat
        , "Url"         =? _sdcUrl
        , "UserBucket"  =? _sdcUserBucket
        ]

data RIProductDescription
    = RIPDLinuxUNIX          -- ^ Linux/UNIX
    | RIPDLinuxUNIXAmazonVPC -- ^ Linux/UNIX (Amazon VPC)
    | RIPDWindows            -- ^ Windows
    | RIPDWindowsAmazonVPC   -- ^ Windows (Amazon VPC)
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable RIProductDescription

instance FromText RIProductDescription where
    parser = takeLowerText >>= \case
        "linux/unix"              -> pure RIPDLinuxUNIX
        "linux/unix (amazon vpc)" -> pure RIPDLinuxUNIXAmazonVPC
        "windows"                 -> pure RIPDWindows
        "windows (amazon vpc)"    -> pure RIPDWindowsAmazonVPC
        e                         -> fail $
            "Failure parsing RIProductDescription from " ++ show e

instance ToText RIProductDescription where
    toText = \case
        RIPDLinuxUNIX          -> "Linux/UNIX"
        RIPDLinuxUNIXAmazonVPC -> "Linux/UNIX (Amazon VPC)"
        RIPDWindows            -> "Windows"
        RIPDWindowsAmazonVPC   -> "Windows (Amazon VPC)"

instance ToByteString RIProductDescription
instance ToHeader     RIProductDescription
instance ToQuery      RIProductDescription

instance FromXML RIProductDescription where
    parseXML = parseXMLText "RIProductDescription"

data ReservedInstancesOffering = ReservedInstancesOffering
    { _rioAvailabilityZone            :: Maybe Text
    , _rioCurrencyCode                :: Maybe CurrencyCodeValues
    , _rioDuration                    :: Maybe Integer
    , _rioFixedPrice                  :: Maybe Double
    , _rioInstanceTenancy             :: Maybe Tenancy
    , _rioInstanceType                :: Maybe InstanceType
    , _rioMarketplace                 :: Maybe Bool
    , _rioOfferingType                :: Maybe OfferingTypeValues
    , _rioPricingDetails              :: List "item" PricingDetail
    , _rioProductDescription          :: Maybe RIProductDescription
    , _rioRecurringCharges            :: List "item" RecurringCharge
    , _rioReservedInstancesOfferingId :: Maybe Text
    , _rioUsagePrice                  :: Maybe Double
    } deriving (Eq, Read, Show)

-- | 'ReservedInstancesOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rioAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'rioCurrencyCode' @::@ 'Maybe' 'CurrencyCodeValues'
--
-- * 'rioDuration' @::@ 'Maybe' 'Integer'
--
-- * 'rioFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rioInstanceTenancy' @::@ 'Maybe' 'Tenancy'
--
-- * 'rioInstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'rioMarketplace' @::@ 'Maybe' 'Bool'
--
-- * 'rioOfferingType' @::@ 'Maybe' 'OfferingTypeValues'
--
-- * 'rioPricingDetails' @::@ ['PricingDetail']
--
-- * 'rioProductDescription' @::@ 'Maybe' 'RIProductDescription'
--
-- * 'rioRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rioReservedInstancesOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rioUsagePrice' @::@ 'Maybe' 'Double'
--
reservedInstancesOffering :: ReservedInstancesOffering
reservedInstancesOffering = ReservedInstancesOffering
    { _rioReservedInstancesOfferingId = Nothing
    , _rioInstanceType                = Nothing
    , _rioAvailabilityZone            = Nothing
    , _rioDuration                    = Nothing
    , _rioUsagePrice                  = Nothing
    , _rioFixedPrice                  = Nothing
    , _rioProductDescription          = Nothing
    , _rioInstanceTenancy             = Nothing
    , _rioCurrencyCode                = Nothing
    , _rioOfferingType                = Nothing
    , _rioRecurringCharges            = mempty
    , _rioMarketplace                 = Nothing
    , _rioPricingDetails              = mempty
    }

-- | The Availability Zone in which the Reserved Instance can be used.
rioAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
rioAvailabilityZone =
    lens _rioAvailabilityZone (\s a -> s { _rioAvailabilityZone = a })

-- | The currency of the Reserved Instance offering you are purchasing. It's
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is 'USD'.
rioCurrencyCode :: Lens' ReservedInstancesOffering (Maybe CurrencyCodeValues)
rioCurrencyCode = lens _rioCurrencyCode (\s a -> s { _rioCurrencyCode = a })

-- | The duration of the Reserved Instance, in seconds.
rioDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
rioDuration = lens _rioDuration (\s a -> s { _rioDuration = a })

-- | The purchase price of the Reserved Instance.
rioFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioFixedPrice = lens _rioFixedPrice (\s a -> s { _rioFixedPrice = a })

-- | The tenancy of the reserved instance.
rioInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Tenancy)
rioInstanceTenancy =
    lens _rioInstanceTenancy (\s a -> s { _rioInstanceTenancy = a })

-- | The instance type on which the Reserved Instance can be used.
rioInstanceType :: Lens' ReservedInstancesOffering (Maybe InstanceType)
rioInstanceType = lens _rioInstanceType (\s a -> s { _rioInstanceType = a })

-- | Indicates whether the offering is available through the Reserved Instance
-- Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace
-- offering, this is 'true'.
rioMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
rioMarketplace = lens _rioMarketplace (\s a -> s { _rioMarketplace = a })

-- | The Reserved Instance offering type.
rioOfferingType :: Lens' ReservedInstancesOffering (Maybe OfferingTypeValues)
rioOfferingType = lens _rioOfferingType (\s a -> s { _rioOfferingType = a })

-- | The pricing details of the Reserved Instance offering.
rioPricingDetails :: Lens' ReservedInstancesOffering [PricingDetail]
rioPricingDetails =
    lens _rioPricingDetails (\s a -> s { _rioPricingDetails = a })
        . _List

-- | The Reserved Instance description.
rioProductDescription :: Lens' ReservedInstancesOffering (Maybe RIProductDescription)
rioProductDescription =
    lens _rioProductDescription (\s a -> s { _rioProductDescription = a })

-- | The recurring charge tag assigned to the resource.
rioRecurringCharges :: Lens' ReservedInstancesOffering [RecurringCharge]
rioRecurringCharges =
    lens _rioRecurringCharges (\s a -> s { _rioRecurringCharges = a })
        . _List

-- | The ID of the Reserved Instance offering.
rioReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
rioReservedInstancesOfferingId =
    lens _rioReservedInstancesOfferingId
        (\s a -> s { _rioReservedInstancesOfferingId = a })

-- | The usage price of the Reserved Instance, per hour.
rioUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioUsagePrice = lens _rioUsagePrice (\s a -> s { _rioUsagePrice = a })

instance FromXML ReservedInstancesOffering where
    parseXML x = ReservedInstancesOffering
        <$> x .@? "availabilityZone"
        <*> x .@? "currencyCode"
        <*> x .@? "duration"
        <*> x .@? "fixedPrice"
        <*> x .@? "instanceTenancy"
        <*> x .@? "instanceType"
        <*> x .@? "marketplace"
        <*> x .@? "offeringType"
        <*> x .@? "pricingDetailsSet" .!@ mempty
        <*> x .@? "productDescription"
        <*> x .@? "recurringCharges" .!@ mempty
        <*> x .@? "reservedInstancesOfferingId"
        <*> x .@? "usagePrice"

instance ToQuery ReservedInstancesOffering where
    toQuery ReservedInstancesOffering{..} = mconcat
        [ "AvailabilityZone"            =? _rioAvailabilityZone
        , "CurrencyCode"                =? _rioCurrencyCode
        , "Duration"                    =? _rioDuration
        , "FixedPrice"                  =? _rioFixedPrice
        , "InstanceTenancy"             =? _rioInstanceTenancy
        , "InstanceType"                =? _rioInstanceType
        , "Marketplace"                 =? _rioMarketplace
        , "OfferingType"                =? _rioOfferingType
        , "PricingDetailsSet"           `toQueryList` _rioPricingDetails
        , "ProductDescription"          =? _rioProductDescription
        , "RecurringCharges"            `toQueryList` _rioRecurringCharges
        , "ReservedInstancesOfferingId" =? _rioReservedInstancesOfferingId
        , "UsagePrice"                  =? _rioUsagePrice
        ]

data ReservedInstances = ReservedInstances
    { _ri1AvailabilityZone    :: Maybe Text
    , _ri1CurrencyCode        :: Maybe CurrencyCodeValues
    , _ri1Duration            :: Maybe Integer
    , _ri1End                 :: Maybe ISO8601
    , _ri1FixedPrice          :: Maybe Double
    , _ri1InstanceCount       :: Maybe Int
    , _ri1InstanceTenancy     :: Maybe Tenancy
    , _ri1InstanceType        :: Maybe InstanceType
    , _ri1OfferingType        :: Maybe OfferingTypeValues
    , _ri1ProductDescription  :: Maybe RIProductDescription
    , _ri1RecurringCharges    :: List "item" RecurringCharge
    , _ri1ReservedInstancesId :: Maybe Text
    , _ri1Start               :: Maybe ISO8601
    , _ri1State               :: Maybe ReservedInstanceState
    , _ri1Tags                :: List "item" Tag
    , _ri1UsagePrice          :: Maybe Double
    } deriving (Eq, Read, Show)

-- | 'ReservedInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ri1AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ri1CurrencyCode' @::@ 'Maybe' 'CurrencyCodeValues'
--
-- * 'ri1Duration' @::@ 'Maybe' 'Integer'
--
-- * 'ri1End' @::@ 'Maybe' 'UTCTime'
--
-- * 'ri1FixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'ri1InstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'ri1InstanceTenancy' @::@ 'Maybe' 'Tenancy'
--
-- * 'ri1InstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'ri1OfferingType' @::@ 'Maybe' 'OfferingTypeValues'
--
-- * 'ri1ProductDescription' @::@ 'Maybe' 'RIProductDescription'
--
-- * 'ri1RecurringCharges' @::@ ['RecurringCharge']
--
-- * 'ri1ReservedInstancesId' @::@ 'Maybe' 'Text'
--
-- * 'ri1Start' @::@ 'Maybe' 'UTCTime'
--
-- * 'ri1State' @::@ 'Maybe' 'ReservedInstanceState'
--
-- * 'ri1Tags' @::@ ['Tag']
--
-- * 'ri1UsagePrice' @::@ 'Maybe' 'Double'
--
reservedInstances :: ReservedInstances
reservedInstances = ReservedInstances
    { _ri1ReservedInstancesId = Nothing
    , _ri1InstanceType        = Nothing
    , _ri1AvailabilityZone    = Nothing
    , _ri1Start               = Nothing
    , _ri1End                 = Nothing
    , _ri1Duration            = Nothing
    , _ri1UsagePrice          = Nothing
    , _ri1FixedPrice          = Nothing
    , _ri1InstanceCount       = Nothing
    , _ri1ProductDescription  = Nothing
    , _ri1State               = Nothing
    , _ri1Tags                = mempty
    , _ri1InstanceTenancy     = Nothing
    , _ri1CurrencyCode        = Nothing
    , _ri1OfferingType        = Nothing
    , _ri1RecurringCharges    = mempty
    }

-- | The Availability Zone in which the Reserved Instance can be used.
ri1AvailabilityZone :: Lens' ReservedInstances (Maybe Text)
ri1AvailabilityZone =
    lens _ri1AvailabilityZone (\s a -> s { _ri1AvailabilityZone = a })

-- | The currency of the Reserved Instance. It's specified using ISO 4217 standard
-- currency codes. At this time, the only supported currency is 'USD'.
ri1CurrencyCode :: Lens' ReservedInstances (Maybe CurrencyCodeValues)
ri1CurrencyCode = lens _ri1CurrencyCode (\s a -> s { _ri1CurrencyCode = a })

-- | The duration of the Reserved Instance, in seconds.
ri1Duration :: Lens' ReservedInstances (Maybe Integer)
ri1Duration = lens _ri1Duration (\s a -> s { _ri1Duration = a })

-- | The time when the Reserved Instance expires.
ri1End :: Lens' ReservedInstances (Maybe UTCTime)
ri1End = lens _ri1End (\s a -> s { _ri1End = a }) . mapping _Time

-- | The purchase price of the Reserved Instance.
ri1FixedPrice :: Lens' ReservedInstances (Maybe Double)
ri1FixedPrice = lens _ri1FixedPrice (\s a -> s { _ri1FixedPrice = a })

-- | The number of Reserved Instances purchased.
ri1InstanceCount :: Lens' ReservedInstances (Maybe Int)
ri1InstanceCount = lens _ri1InstanceCount (\s a -> s { _ri1InstanceCount = a })

-- | The tenancy of the reserved instance.
ri1InstanceTenancy :: Lens' ReservedInstances (Maybe Tenancy)
ri1InstanceTenancy =
    lens _ri1InstanceTenancy (\s a -> s { _ri1InstanceTenancy = a })

-- | The instance type on which the Reserved Instance can be used.
ri1InstanceType :: Lens' ReservedInstances (Maybe InstanceType)
ri1InstanceType = lens _ri1InstanceType (\s a -> s { _ri1InstanceType = a })

-- | The Reserved Instance offering type.
ri1OfferingType :: Lens' ReservedInstances (Maybe OfferingTypeValues)
ri1OfferingType = lens _ri1OfferingType (\s a -> s { _ri1OfferingType = a })

-- | The Reserved Instance description.
ri1ProductDescription :: Lens' ReservedInstances (Maybe RIProductDescription)
ri1ProductDescription =
    lens _ri1ProductDescription (\s a -> s { _ri1ProductDescription = a })

-- | The recurring charge tag assigned to the resource.
ri1RecurringCharges :: Lens' ReservedInstances [RecurringCharge]
ri1RecurringCharges =
    lens _ri1RecurringCharges (\s a -> s { _ri1RecurringCharges = a })
        . _List

-- | The ID of the Reserved Instance.
ri1ReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
ri1ReservedInstancesId =
    lens _ri1ReservedInstancesId (\s a -> s { _ri1ReservedInstancesId = a })

-- | The date and time the Reserved Instance started.
ri1Start :: Lens' ReservedInstances (Maybe UTCTime)
ri1Start = lens _ri1Start (\s a -> s { _ri1Start = a }) . mapping _Time

-- | The state of the Reserved Instance purchase.
ri1State :: Lens' ReservedInstances (Maybe ReservedInstanceState)
ri1State = lens _ri1State (\s a -> s { _ri1State = a })

-- | Any tags assigned to the resource.
ri1Tags :: Lens' ReservedInstances [Tag]
ri1Tags = lens _ri1Tags (\s a -> s { _ri1Tags = a }) . _List

-- | The usage price of the Reserved Instance, per hour.
ri1UsagePrice :: Lens' ReservedInstances (Maybe Double)
ri1UsagePrice = lens _ri1UsagePrice (\s a -> s { _ri1UsagePrice = a })

instance FromXML ReservedInstances where
    parseXML x = ReservedInstances
        <$> x .@? "availabilityZone"
        <*> x .@? "currencyCode"
        <*> x .@? "duration"
        <*> x .@? "end"
        <*> x .@? "fixedPrice"
        <*> x .@? "instanceCount"
        <*> x .@? "instanceTenancy"
        <*> x .@? "instanceType"
        <*> x .@? "offeringType"
        <*> x .@? "productDescription"
        <*> x .@? "recurringCharges" .!@ mempty
        <*> x .@? "reservedInstancesId"
        <*> x .@? "start"
        <*> x .@? "state"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "usagePrice"

instance ToQuery ReservedInstances where
    toQuery ReservedInstances{..} = mconcat
        [ "AvailabilityZone"    =? _ri1AvailabilityZone
        , "CurrencyCode"        =? _ri1CurrencyCode
        , "Duration"            =? _ri1Duration
        , "End"                 =? _ri1End
        , "FixedPrice"          =? _ri1FixedPrice
        , "InstanceCount"       =? _ri1InstanceCount
        , "InstanceTenancy"     =? _ri1InstanceTenancy
        , "InstanceType"        =? _ri1InstanceType
        , "OfferingType"        =? _ri1OfferingType
        , "ProductDescription"  =? _ri1ProductDescription
        , "RecurringCharges"    `toQueryList` _ri1RecurringCharges
        , "ReservedInstancesId" =? _ri1ReservedInstancesId
        , "Start"               =? _ri1Start
        , "State"               =? _ri1State
        , "TagSet"              `toQueryList` _ri1Tags
        , "UsagePrice"          =? _ri1UsagePrice
        ]

data DatafeedSubscriptionState
    = DSSActive   -- ^ Active
    | DSSInactive -- ^ Inactive
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DatafeedSubscriptionState

instance FromText DatafeedSubscriptionState where
    parser = takeLowerText >>= \case
        "active"   -> pure DSSActive
        "inactive" -> pure DSSInactive
        e          -> fail $
            "Failure parsing DatafeedSubscriptionState from " ++ show e

instance ToText DatafeedSubscriptionState where
    toText = \case
        DSSActive   -> "Active"
        DSSInactive -> "Inactive"

instance ToByteString DatafeedSubscriptionState
instance ToHeader     DatafeedSubscriptionState
instance ToQuery      DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
    parseXML = parseXMLText "DatafeedSubscriptionState"

data ExportTaskState
    = ETSActive     -- ^ active
    | ETSCancelled  -- ^ cancelled
    | ETSCancelling -- ^ cancelling
    | ETSCompleted  -- ^ completed
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ExportTaskState

instance FromText ExportTaskState where
    parser = takeLowerText >>= \case
        "active"     -> pure ETSActive
        "cancelled"  -> pure ETSCancelled
        "cancelling" -> pure ETSCancelling
        "completed"  -> pure ETSCompleted
        e            -> fail $
            "Failure parsing ExportTaskState from " ++ show e

instance ToText ExportTaskState where
    toText = \case
        ETSActive     -> "active"
        ETSCancelled  -> "cancelled"
        ETSCancelling -> "cancelling"
        ETSCompleted  -> "completed"

instance ToByteString ExportTaskState
instance ToHeader     ExportTaskState
instance ToQuery      ExportTaskState

instance FromXML ExportTaskState where
    parseXML = parseXMLText "ExportTaskState"

data ProductCodeValues
    = Devpay      -- ^ devpay
    | Marketplace -- ^ marketplace
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ProductCodeValues

instance FromText ProductCodeValues where
    parser = takeLowerText >>= \case
        "devpay"      -> pure Devpay
        "marketplace" -> pure Marketplace
        e             -> fail $
            "Failure parsing ProductCodeValues from " ++ show e

instance ToText ProductCodeValues where
    toText = \case
        Devpay      -> "devpay"
        Marketplace -> "marketplace"

instance ToByteString ProductCodeValues
instance ToHeader     ProductCodeValues
instance ToQuery      ProductCodeValues

instance FromXML ProductCodeValues where
    parseXML = parseXMLText "ProductCodeValues"

data VpnConnection = VpnConnection
    { _vcCustomerGatewayConfiguration :: Text
    , _vcCustomerGatewayId            :: Text
    , _vcOptions                      :: Maybe VpnConnectionOptions
    , _vcRoutes                       :: List "item" VpnStaticRoute
    , _vcState                        :: VpnState
    , _vcTags                         :: List "item" Tag
    , _vcType                         :: GatewayType
    , _vcVgwTelemetry                 :: List "item" VgwTelemetry
    , _vcVpnConnectionId              :: Text
    , _vcVpnGatewayId                 :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VpnConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcCustomerGatewayConfiguration' @::@ 'Text'
--
-- * 'vcCustomerGatewayId' @::@ 'Text'
--
-- * 'vcOptions' @::@ 'Maybe' 'VpnConnectionOptions'
--
-- * 'vcRoutes' @::@ ['VpnStaticRoute']
--
-- * 'vcState' @::@ 'VpnState'
--
-- * 'vcTags' @::@ ['Tag']
--
-- * 'vcType' @::@ 'GatewayType'
--
-- * 'vcVgwTelemetry' @::@ ['VgwTelemetry']
--
-- * 'vcVpnConnectionId' @::@ 'Text'
--
-- * 'vcVpnGatewayId' @::@ 'Maybe' 'Text'
--
vpnConnection :: Text -- ^ 'vcVpnConnectionId'
              -> VpnState -- ^ 'vcState'
              -> Text -- ^ 'vcCustomerGatewayConfiguration'
              -> GatewayType -- ^ 'vcType'
              -> Text -- ^ 'vcCustomerGatewayId'
              -> VpnConnection
vpnConnection p1 p2 p3 p4 p5 = VpnConnection
    { _vcVpnConnectionId              = p1
    , _vcState                        = p2
    , _vcCustomerGatewayConfiguration = p3
    , _vcType                         = p4
    , _vcCustomerGatewayId            = p5
    , _vcVpnGatewayId                 = Nothing
    , _vcTags                         = mempty
    , _vcVgwTelemetry                 = mempty
    , _vcOptions                      = Nothing
    , _vcRoutes                       = mempty
    }

-- | The configuration information for the VPN connection's customer gateway (in
-- the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only
-- if the VPN connection is in the 'pending' or 'available' state.
vcCustomerGatewayConfiguration :: Lens' VpnConnection Text
vcCustomerGatewayConfiguration =
    lens _vcCustomerGatewayConfiguration
        (\s a -> s { _vcCustomerGatewayConfiguration = a })

-- | The ID of the customer gateway at your end of the VPN connection.
vcCustomerGatewayId :: Lens' VpnConnection Text
vcCustomerGatewayId =
    lens _vcCustomerGatewayId (\s a -> s { _vcCustomerGatewayId = a })

-- | The VPN connection options.
vcOptions :: Lens' VpnConnection (Maybe VpnConnectionOptions)
vcOptions = lens _vcOptions (\s a -> s { _vcOptions = a })

-- | The static routes associated with the VPN connection.
vcRoutes :: Lens' VpnConnection [VpnStaticRoute]
vcRoutes = lens _vcRoutes (\s a -> s { _vcRoutes = a }) . _List

-- | The current state of the VPN connection.
vcState :: Lens' VpnConnection VpnState
vcState = lens _vcState (\s a -> s { _vcState = a })

-- | Any tags assigned to the VPN connection.
vcTags :: Lens' VpnConnection [Tag]
vcTags = lens _vcTags (\s a -> s { _vcTags = a }) . _List

-- | The type of VPN connection.
vcType :: Lens' VpnConnection GatewayType
vcType = lens _vcType (\s a -> s { _vcType = a })

-- | Information about the VPN tunnel.
vcVgwTelemetry :: Lens' VpnConnection [VgwTelemetry]
vcVgwTelemetry = lens _vcVgwTelemetry (\s a -> s { _vcVgwTelemetry = a }) . _List

-- | The ID of the VPN connection.
vcVpnConnectionId :: Lens' VpnConnection Text
vcVpnConnectionId =
    lens _vcVpnConnectionId (\s a -> s { _vcVpnConnectionId = a })

-- | The ID of the virtual private gateway at the AWS side of the VPN connection.
vcVpnGatewayId :: Lens' VpnConnection (Maybe Text)
vcVpnGatewayId = lens _vcVpnGatewayId (\s a -> s { _vcVpnGatewayId = a })

instance FromXML VpnConnection where
    parseXML x = VpnConnection
        <$> x .@  "customerGatewayConfiguration"
        <*> x .@  "customerGatewayId"
        <*> x .@? "options"
        <*> x .@? "routes" .!@ mempty
        <*> x .@  "state"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "type"
        <*> x .@? "vgwTelemetry" .!@ mempty
        <*> x .@  "vpnConnectionId"
        <*> x .@? "vpnGatewayId"

instance ToQuery VpnConnection where
    toQuery VpnConnection{..} = mconcat
        [ "CustomerGatewayConfiguration" =? _vcCustomerGatewayConfiguration
        , "CustomerGatewayId"            =? _vcCustomerGatewayId
        , "Options"                      =? _vcOptions
        , "Routes"                       `toQueryList` _vcRoutes
        , "State"                        =? _vcState
        , "TagSet"                       `toQueryList` _vcTags
        , "Type"                         =? _vcType
        , "VgwTelemetry"                 `toQueryList` _vcVgwTelemetry
        , "VpnConnectionId"              =? _vcVpnConnectionId
        , "VpnGatewayId"                 =? _vcVpnGatewayId
        ]

data InstanceState = InstanceState
    { _isCode :: Int
    , _isName :: InstanceStateName
    } deriving (Eq, Read, Show)

-- | 'InstanceState' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isCode' @::@ 'Int'
--
-- * 'isName' @::@ 'InstanceStateName'
--
instanceState :: Int -- ^ 'isCode'
              -> InstanceStateName -- ^ 'isName'
              -> InstanceState
instanceState p1 p2 = InstanceState
    { _isCode = p1
    , _isName = p2
    }

-- | The low byte represents the state. The high byte is an opaque internal value
-- and should be ignored.
--
-- '0' : 'pending'
--
-- '16' : 'running'
--
-- '32' : 'shutting-down'
--
-- '48' : 'terminated'
--
-- '64' : 'stopping'
--
-- '80' : 'stopped'
--
--
isCode :: Lens' InstanceState Int
isCode = lens _isCode (\s a -> s { _isCode = a })

-- | The current state of the instance.
isName :: Lens' InstanceState InstanceStateName
isName = lens _isName (\s a -> s { _isName = a })

instance FromXML InstanceState where
    parseXML x = InstanceState
        <$> x .@  "code"
        <*> x .@  "name"

instance ToQuery InstanceState where
    toQuery InstanceState{..} = mconcat
        [ "Code" =? _isCode
        , "Name" =? _isName
        ]

data VpcEndpoint = VpcEndpoint
    { _veCreationTimestamp :: Maybe ISO8601
    , _vePolicyDocument    :: Maybe Text
    , _veRouteTableIds     :: List "item" Text
    , _veServiceName       :: Maybe Text
    , _veState             :: Maybe State
    , _veVpcEndpointId     :: Maybe Text
    , _veVpcId             :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VpcEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veCreationTimestamp' @::@ 'Maybe' 'UTCTime'
--
-- * 'vePolicyDocument' @::@ 'Maybe' 'Text'
--
-- * 'veRouteTableIds' @::@ ['Text']
--
-- * 'veServiceName' @::@ 'Maybe' 'Text'
--
-- * 'veState' @::@ 'Maybe' 'State'
--
-- * 'veVpcEndpointId' @::@ 'Maybe' 'Text'
--
-- * 'veVpcId' @::@ 'Maybe' 'Text'
--
vpcEndpoint :: VpcEndpoint
vpcEndpoint = VpcEndpoint
    { _veVpcEndpointId     = Nothing
    , _veVpcId             = Nothing
    , _veServiceName       = Nothing
    , _veState             = Nothing
    , _vePolicyDocument    = Nothing
    , _veRouteTableIds     = mempty
    , _veCreationTimestamp = Nothing
    }

-- | The date and time the VPC endpoint was created.
veCreationTimestamp :: Lens' VpcEndpoint (Maybe UTCTime)
veCreationTimestamp =
    lens _veCreationTimestamp (\s a -> s { _veCreationTimestamp = a })
        . mapping _Time

-- | The policy document associated with the endpoint.
vePolicyDocument :: Lens' VpcEndpoint (Maybe Text)
vePolicyDocument = lens _vePolicyDocument (\s a -> s { _vePolicyDocument = a })

-- | One or more route tables associated with the endpoint.
veRouteTableIds :: Lens' VpcEndpoint [Text]
veRouteTableIds = lens _veRouteTableIds (\s a -> s { _veRouteTableIds = a }) . _List

-- | The name of the AWS service to which the endpoint is associated.
veServiceName :: Lens' VpcEndpoint (Maybe Text)
veServiceName = lens _veServiceName (\s a -> s { _veServiceName = a })

-- | The state of the VPC endpoint.
veState :: Lens' VpcEndpoint (Maybe State)
veState = lens _veState (\s a -> s { _veState = a })

-- | The ID of the VPC endpoint.
veVpcEndpointId :: Lens' VpcEndpoint (Maybe Text)
veVpcEndpointId = lens _veVpcEndpointId (\s a -> s { _veVpcEndpointId = a })

-- | The ID of the VPC to which the endpoint is associated.
veVpcId :: Lens' VpcEndpoint (Maybe Text)
veVpcId = lens _veVpcId (\s a -> s { _veVpcId = a })

instance FromXML VpcEndpoint where
    parseXML x = VpcEndpoint
        <$> x .@? "creationTimestamp"
        <*> x .@? "policyDocument"
        <*> x .@? "routeTableIdSet" .!@ mempty
        <*> x .@? "serviceName"
        <*> x .@? "state"
        <*> x .@? "vpcEndpointId"
        <*> x .@? "vpcId"

instance ToQuery VpcEndpoint where
    toQuery VpcEndpoint{..} = mconcat
        [ "CreationTimestamp" =? _veCreationTimestamp
        , "PolicyDocument"    =? _vePolicyDocument
        , "RouteTableIdSet"   `toQueryList` _veRouteTableIds
        , "ServiceName"       =? _veServiceName
        , "State"             =? _veState
        , "VpcEndpointId"     =? _veVpcEndpointId
        , "VpcId"             =? _veVpcId
        ]

data ClientData = ClientData
    { _cdComment     :: Maybe Text
    , _cdUploadEnd   :: Maybe ISO8601
    , _cdUploadSize  :: Maybe Double
    , _cdUploadStart :: Maybe ISO8601
    } deriving (Eq, Ord, Read, Show)

-- | 'ClientData' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdComment' @::@ 'Maybe' 'Text'
--
-- * 'cdUploadEnd' @::@ 'Maybe' 'UTCTime'
--
-- * 'cdUploadSize' @::@ 'Maybe' 'Double'
--
-- * 'cdUploadStart' @::@ 'Maybe' 'UTCTime'
--
clientData :: ClientData
clientData = ClientData
    { _cdUploadStart = Nothing
    , _cdUploadEnd   = Nothing
    , _cdUploadSize  = Nothing
    , _cdComment     = Nothing
    }

-- | A user-defined comment about the disk upload.
cdComment :: Lens' ClientData (Maybe Text)
cdComment = lens _cdComment (\s a -> s { _cdComment = a })

-- | The time that the disk upload ends.
cdUploadEnd :: Lens' ClientData (Maybe UTCTime)
cdUploadEnd = lens _cdUploadEnd (\s a -> s { _cdUploadEnd = a }) . mapping _Time

-- | The size of the uploaded disk image, in GiB.
cdUploadSize :: Lens' ClientData (Maybe Double)
cdUploadSize = lens _cdUploadSize (\s a -> s { _cdUploadSize = a })

-- | The time that the disk upload starts.
cdUploadStart :: Lens' ClientData (Maybe UTCTime)
cdUploadStart = lens _cdUploadStart (\s a -> s { _cdUploadStart = a }) . mapping _Time

instance FromXML ClientData where
    parseXML x = ClientData
        <$> x .@? "Comment"
        <*> x .@? "UploadEnd"
        <*> x .@? "UploadSize"
        <*> x .@? "UploadStart"

instance ToQuery ClientData where
    toQuery ClientData{..} = mconcat
        [ "Comment"     =? _cdComment
        , "UploadEnd"   =? _cdUploadEnd
        , "UploadSize"  =? _cdUploadSize
        , "UploadStart" =? _cdUploadStart
        ]

data Placement = Placement
    { _pAvailabilityZone :: Maybe Text
    , _pGroupName        :: Maybe Text
    , _pTenancy          :: Maybe Tenancy
    } deriving (Eq, Read, Show)

-- | 'Placement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'pGroupName' @::@ 'Maybe' 'Text'
--
-- * 'pTenancy' @::@ 'Maybe' 'Tenancy'
--
placement :: Placement
placement = Placement
    { _pAvailabilityZone = Nothing
    , _pGroupName        = Nothing
    , _pTenancy          = Nothing
    }

-- | The Availability Zone of the instance.
pAvailabilityZone :: Lens' Placement (Maybe Text)
pAvailabilityZone =
    lens _pAvailabilityZone (\s a -> s { _pAvailabilityZone = a })

-- | The name of the placement group the instance is in (for cluster compute
-- instances).
pGroupName :: Lens' Placement (Maybe Text)
pGroupName = lens _pGroupName (\s a -> s { _pGroupName = a })

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of 'dedicated' runs on single-tenant hardware.
pTenancy :: Lens' Placement (Maybe Tenancy)
pTenancy = lens _pTenancy (\s a -> s { _pTenancy = a })

instance FromXML Placement where
    parseXML x = Placement
        <$> x .@? "availabilityZone"
        <*> x .@? "groupName"
        <*> x .@? "tenancy"

instance ToQuery Placement where
    toQuery Placement{..} = mconcat
        [ "AvailabilityZone" =? _pAvailabilityZone
        , "GroupName"        =? _pGroupName
        , "Tenancy"          =? _pTenancy
        ]

data EventCode
    = InstanceReboot     -- ^ instance-reboot
    | InstanceRetirement -- ^ instance-retirement
    | InstanceStop       -- ^ instance-stop
    | SystemMaintenance  -- ^ system-maintenance
    | SystemReboot       -- ^ system-reboot
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable EventCode

instance FromText EventCode where
    parser = takeLowerText >>= \case
        "instance-reboot"     -> pure InstanceReboot
        "instance-retirement" -> pure InstanceRetirement
        "instance-stop"       -> pure InstanceStop
        "system-maintenance"  -> pure SystemMaintenance
        "system-reboot"       -> pure SystemReboot
        e                     -> fail $
            "Failure parsing EventCode from " ++ show e

instance ToText EventCode where
    toText = \case
        InstanceReboot     -> "instance-reboot"
        InstanceRetirement -> "instance-retirement"
        InstanceStop       -> "instance-stop"
        SystemMaintenance  -> "system-maintenance"
        SystemReboot       -> "system-reboot"

instance ToByteString EventCode
instance ToHeader     EventCode
instance ToQuery      EventCode

instance FromXML EventCode where
    parseXML = parseXMLText "EventCode"

data SpotInstanceType
    = OneTime    -- ^ one-time
    | Persistent -- ^ persistent
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SpotInstanceType

instance FromText SpotInstanceType where
    parser = takeLowerText >>= \case
        "one-time"   -> pure OneTime
        "persistent" -> pure Persistent
        e            -> fail $
            "Failure parsing SpotInstanceType from " ++ show e

instance ToText SpotInstanceType where
    toText = \case
        OneTime    -> "one-time"
        Persistent -> "persistent"

instance ToByteString SpotInstanceType
instance ToHeader     SpotInstanceType
instance ToQuery      SpotInstanceType

instance FromXML SpotInstanceType where
    parseXML = parseXMLText "SpotInstanceType"

data VpcPeeringConnection = VpcPeeringConnection
    { _vpc1AccepterVpcInfo        :: Maybe VpcPeeringConnectionVpcInfo
    , _vpc1ExpirationTime         :: Maybe ISO8601
    , _vpc1RequesterVpcInfo       :: Maybe VpcPeeringConnectionVpcInfo
    , _vpc1Status                 :: Maybe VpcPeeringConnectionStateReason
    , _vpc1Tags                   :: List "item" Tag
    , _vpc1VpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VpcPeeringConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpc1AccepterVpcInfo' @::@ 'Maybe' 'VpcPeeringConnectionVpcInfo'
--
-- * 'vpc1ExpirationTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'vpc1RequesterVpcInfo' @::@ 'Maybe' 'VpcPeeringConnectionVpcInfo'
--
-- * 'vpc1Status' @::@ 'Maybe' 'VpcPeeringConnectionStateReason'
--
-- * 'vpc1Tags' @::@ ['Tag']
--
-- * 'vpc1VpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
vpcPeeringConnection :: VpcPeeringConnection
vpcPeeringConnection = VpcPeeringConnection
    { _vpc1AccepterVpcInfo        = Nothing
    , _vpc1ExpirationTime         = Nothing
    , _vpc1RequesterVpcInfo       = Nothing
    , _vpc1Status                 = Nothing
    , _vpc1Tags                   = mempty
    , _vpc1VpcPeeringConnectionId = Nothing
    }

-- | The information of the peer VPC.
vpc1AccepterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpc1AccepterVpcInfo =
    lens _vpc1AccepterVpcInfo (\s a -> s { _vpc1AccepterVpcInfo = a })

-- | The time that an unaccepted VPC peering connection will expire.
vpc1ExpirationTime :: Lens' VpcPeeringConnection (Maybe UTCTime)
vpc1ExpirationTime =
    lens _vpc1ExpirationTime (\s a -> s { _vpc1ExpirationTime = a })
        . mapping _Time

-- | The information of the requester VPC.
vpc1RequesterVpcInfo :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionVpcInfo)
vpc1RequesterVpcInfo =
    lens _vpc1RequesterVpcInfo (\s a -> s { _vpc1RequesterVpcInfo = a })

-- | The status of the VPC peering connection.
vpc1Status :: Lens' VpcPeeringConnection (Maybe VpcPeeringConnectionStateReason)
vpc1Status = lens _vpc1Status (\s a -> s { _vpc1Status = a })

-- | Any tags assigned to the resource.
vpc1Tags :: Lens' VpcPeeringConnection [Tag]
vpc1Tags = lens _vpc1Tags (\s a -> s { _vpc1Tags = a }) . _List

-- | The ID of the VPC peering connection.
vpc1VpcPeeringConnectionId :: Lens' VpcPeeringConnection (Maybe Text)
vpc1VpcPeeringConnectionId =
    lens _vpc1VpcPeeringConnectionId
        (\s a -> s { _vpc1VpcPeeringConnectionId = a })

instance FromXML VpcPeeringConnection where
    parseXML x = VpcPeeringConnection
        <$> x .@? "accepterVpcInfo"
        <*> x .@? "expirationTime"
        <*> x .@? "requesterVpcInfo"
        <*> x .@? "status"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "vpcPeeringConnectionId"

instance ToQuery VpcPeeringConnection where
    toQuery VpcPeeringConnection{..} = mconcat
        [ "AccepterVpcInfo"        =? _vpc1AccepterVpcInfo
        , "ExpirationTime"         =? _vpc1ExpirationTime
        , "RequesterVpcInfo"       =? _vpc1RequesterVpcInfo
        , "Status"                 =? _vpc1Status
        , "TagSet"                 `toQueryList` _vpc1Tags
        , "VpcPeeringConnectionId" =? _vpc1VpcPeeringConnectionId
        ]

data S3Storage = S3Storage
    { _ssAWSAccessKeyId        :: Maybe Text
    , _ssBucket                :: Maybe Text
    , _ssPrefix                :: Maybe Text
    , _ssUploadPolicy          :: Maybe Base64
    , _ssUploadPolicySignature :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'S3Storage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssAWSAccessKeyId' @::@ 'Maybe' 'Text'
--
-- * 'ssBucket' @::@ 'Maybe' 'Text'
--
-- * 'ssPrefix' @::@ 'Maybe' 'Text'
--
-- * 'ssUploadPolicy' @::@ 'Maybe' 'Base64'
--
-- * 'ssUploadPolicySignature' @::@ 'Maybe' 'Text'
--
s3Storage :: S3Storage
s3Storage = S3Storage
    { _ssBucket                = Nothing
    , _ssPrefix                = Nothing
    , _ssAWSAccessKeyId        = Nothing
    , _ssUploadPolicy          = Nothing
    , _ssUploadPolicySignature = Nothing
    }

-- | The access key ID of the owner of the bucket. Before you specify a value for
-- your access key ID, review and follow the guidance in <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices forManaging AWS Access Keys>.
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId = lens _ssAWSAccessKeyId (\s a -> s { _ssAWSAccessKeyId = a })

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If you
-- specify a bucket that belongs to someone else, Amazon EC2 returns an error.
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket = lens _ssBucket (\s a -> s { _ssBucket = a })

-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix = lens _ssPrefix (\s a -> s { _ssPrefix = a })

-- | A Base64-encoded Amazon S3 upload policy that gives Amazon EC2 permission to
-- upload items into Amazon S3 on your behalf.
ssUploadPolicy :: Lens' S3Storage (Maybe Base64)
ssUploadPolicy = lens _ssUploadPolicy (\s a -> s { _ssUploadPolicy = a })

-- | The signature of the Base64 encoded JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature =
    lens _ssUploadPolicySignature (\s a -> s { _ssUploadPolicySignature = a })

instance FromXML S3Storage where
    parseXML x = S3Storage
        <$> x .@? "AWSAccessKeyId"
        <*> x .@? "bucket"
        <*> x .@? "prefix"
        <*> x .@? "uploadPolicy"
        <*> x .@? "uploadPolicySignature"

instance ToQuery S3Storage where
    toQuery S3Storage{..} = mconcat
        [ "AWSAccessKeyId"        =? _ssAWSAccessKeyId
        , "Bucket"                =? _ssBucket
        , "Prefix"                =? _ssPrefix
        , "UploadPolicy"          =? _ssUploadPolicy
        , "UploadPolicySignature" =? _ssUploadPolicySignature
        ]

data VgwTelemetry = VgwTelemetry
    { _vtAcceptedRouteCount :: Maybe Int
    , _vtLastStatusChange   :: Maybe ISO8601
    , _vtOutsideIpAddress   :: Maybe Text
    , _vtStatus             :: Maybe TelemetryStatus
    , _vtStatusMessage      :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VgwTelemetry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtAcceptedRouteCount' @::@ 'Maybe' 'Int'
--
-- * 'vtLastStatusChange' @::@ 'Maybe' 'UTCTime'
--
-- * 'vtOutsideIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'vtStatus' @::@ 'Maybe' 'TelemetryStatus'
--
-- * 'vtStatusMessage' @::@ 'Maybe' 'Text'
--
vgwTelemetry :: VgwTelemetry
vgwTelemetry = VgwTelemetry
    { _vtOutsideIpAddress   = Nothing
    , _vtStatus             = Nothing
    , _vtLastStatusChange   = Nothing
    , _vtStatusMessage      = Nothing
    , _vtAcceptedRouteCount = Nothing
    }

-- | The number of accepted routes.
vtAcceptedRouteCount :: Lens' VgwTelemetry (Maybe Int)
vtAcceptedRouteCount =
    lens _vtAcceptedRouteCount (\s a -> s { _vtAcceptedRouteCount = a })

-- | The date and time of the last change in status.
vtLastStatusChange :: Lens' VgwTelemetry (Maybe UTCTime)
vtLastStatusChange =
    lens _vtLastStatusChange (\s a -> s { _vtLastStatusChange = a })
        . mapping _Time

-- | The Internet-routable IP address of the virtual private gateway's outside
-- interface.
vtOutsideIpAddress :: Lens' VgwTelemetry (Maybe Text)
vtOutsideIpAddress =
    lens _vtOutsideIpAddress (\s a -> s { _vtOutsideIpAddress = a })

-- | The status of the VPN tunnel.
vtStatus :: Lens' VgwTelemetry (Maybe TelemetryStatus)
vtStatus = lens _vtStatus (\s a -> s { _vtStatus = a })

-- | If an error occurs, a description of the error.
vtStatusMessage :: Lens' VgwTelemetry (Maybe Text)
vtStatusMessage = lens _vtStatusMessage (\s a -> s { _vtStatusMessage = a })

instance FromXML VgwTelemetry where
    parseXML x = VgwTelemetry
        <$> x .@? "acceptedRouteCount"
        <*> x .@? "lastStatusChange"
        <*> x .@? "outsideIpAddress"
        <*> x .@? "status"
        <*> x .@? "statusMessage"

instance ToQuery VgwTelemetry where
    toQuery VgwTelemetry{..} = mconcat
        [ "AcceptedRouteCount" =? _vtAcceptedRouteCount
        , "LastStatusChange"   =? _vtLastStatusChange
        , "OutsideIpAddress"   =? _vtOutsideIpAddress
        , "Status"             =? _vtStatus
        , "StatusMessage"      =? _vtStatusMessage
        ]

data VpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock :: Maybe Text
    , _vsrSource               :: Maybe VpnStaticRouteSource
    , _vsrState                :: Maybe VpnState
    } deriving (Eq, Read, Show)

-- | 'VpnStaticRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsrDestinationCidrBlock' @::@ 'Maybe' 'Text'
--
-- * 'vsrSource' @::@ 'Maybe' 'VpnStaticRouteSource'
--
-- * 'vsrState' @::@ 'Maybe' 'VpnState'
--
vpnStaticRoute :: VpnStaticRoute
vpnStaticRoute = VpnStaticRoute
    { _vsrDestinationCidrBlock = Nothing
    , _vsrSource               = Nothing
    , _vsrState                = Nothing
    }

-- | The CIDR block associated with the local subnet of the customer data center.
vsrDestinationCidrBlock :: Lens' VpnStaticRoute (Maybe Text)
vsrDestinationCidrBlock =
    lens _vsrDestinationCidrBlock (\s a -> s { _vsrDestinationCidrBlock = a })

-- | Indicates how the routes were provided.
vsrSource :: Lens' VpnStaticRoute (Maybe VpnStaticRouteSource)
vsrSource = lens _vsrSource (\s a -> s { _vsrSource = a })

-- | The current state of the static route.
vsrState :: Lens' VpnStaticRoute (Maybe VpnState)
vsrState = lens _vsrState (\s a -> s { _vsrState = a })

instance FromXML VpnStaticRoute where
    parseXML x = VpnStaticRoute
        <$> x .@? "destinationCidrBlock"
        <*> x .@? "source"
        <*> x .@? "state"

instance ToQuery VpnStaticRoute where
    toQuery VpnStaticRoute{..} = mconcat
        [ "DestinationCidrBlock" =? _vsrDestinationCidrBlock
        , "Source"               =? _vsrSource
        , "State"                =? _vsrState
        ]

data InstanceStateName
    = ISNPending      -- ^ pending
    | ISNRunning      -- ^ running
    | ISNShuttingDown -- ^ shutting-down
    | ISNStopped      -- ^ stopped
    | ISNStopping     -- ^ stopping
    | ISNTerminated   -- ^ terminated
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable InstanceStateName

instance FromText InstanceStateName where
    parser = takeLowerText >>= \case
        "pending"       -> pure ISNPending
        "running"       -> pure ISNRunning
        "shutting-down" -> pure ISNShuttingDown
        "stopped"       -> pure ISNStopped
        "stopping"      -> pure ISNStopping
        "terminated"    -> pure ISNTerminated
        e               -> fail $
            "Failure parsing InstanceStateName from " ++ show e

instance ToText InstanceStateName where
    toText = \case
        ISNPending      -> "pending"
        ISNRunning      -> "running"
        ISNShuttingDown -> "shutting-down"
        ISNStopped      -> "stopped"
        ISNStopping     -> "stopping"
        ISNTerminated   -> "terminated"

instance ToByteString InstanceStateName
instance ToHeader     InstanceStateName
instance ToQuery      InstanceStateName

instance FromXML InstanceStateName where
    parseXML = parseXMLText "InstanceStateName"

data Instance = Instance
    { _i1AmiLaunchIndex        :: Int
    , _i1Architecture          :: ArchitectureValues
    , _i1BlockDeviceMappings   :: List "item" InstanceBlockDeviceMapping
    , _i1ClientToken           :: Maybe Text
    , _i1EbsOptimized          :: Bool
    , _i1Hypervisor            :: HypervisorType
    , _i1IamInstanceProfile    :: Maybe IamInstanceProfile
    , _i1ImageId               :: Text
    , _i1InstanceId            :: Text
    , _i1InstanceLifecycle     :: Maybe InstanceLifecycleType
    , _i1InstanceType          :: InstanceType
    , _i1KernelId              :: Maybe Text
    , _i1KeyName               :: Maybe Text
    , _i1LaunchTime            :: ISO8601
    , _i1Monitoring            :: Monitoring
    , _i1NetworkInterfaces     :: List "item" InstanceNetworkInterface
    , _i1Placement             :: Placement
    , _i1Platform              :: Maybe PlatformValues
    , _i1PrivateDnsName        :: Maybe Text
    , _i1PrivateIpAddress      :: Maybe Text
    , _i1ProductCodes          :: List "item" ProductCode
    , _i1PublicDnsName         :: Maybe Text
    , _i1PublicIpAddress       :: Maybe Text
    , _i1RamdiskId             :: Maybe Text
    , _i1RootDeviceName        :: Maybe Text
    , _i1RootDeviceType        :: DeviceType
    , _i1SecurityGroups        :: List "item" GroupIdentifier
    , _i1SourceDestCheck       :: Maybe Bool
    , _i1SpotInstanceRequestId :: Maybe Text
    , _i1SriovNetSupport       :: Maybe Text
    , _i1State                 :: InstanceState
    , _i1StateReason           :: Maybe StateReason
    , _i1StateTransitionReason :: Maybe Text
    , _i1SubnetId              :: Maybe Text
    , _i1Tags                  :: List "item" Tag
    , _i1VirtualizationType    :: VirtualizationType
    , _i1VpcId                 :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Instance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'i1AmiLaunchIndex' @::@ 'Int'
--
-- * 'i1Architecture' @::@ 'ArchitectureValues'
--
-- * 'i1BlockDeviceMappings' @::@ ['InstanceBlockDeviceMapping']
--
-- * 'i1ClientToken' @::@ 'Maybe' 'Text'
--
-- * 'i1EbsOptimized' @::@ 'Bool'
--
-- * 'i1Hypervisor' @::@ 'HypervisorType'
--
-- * 'i1IamInstanceProfile' @::@ 'Maybe' 'IamInstanceProfile'
--
-- * 'i1ImageId' @::@ 'Text'
--
-- * 'i1InstanceId' @::@ 'Text'
--
-- * 'i1InstanceLifecycle' @::@ 'Maybe' 'InstanceLifecycleType'
--
-- * 'i1InstanceType' @::@ 'InstanceType'
--
-- * 'i1KernelId' @::@ 'Maybe' 'Text'
--
-- * 'i1KeyName' @::@ 'Maybe' 'Text'
--
-- * 'i1LaunchTime' @::@ 'UTCTime'
--
-- * 'i1Monitoring' @::@ 'Monitoring'
--
-- * 'i1NetworkInterfaces' @::@ ['InstanceNetworkInterface']
--
-- * 'i1Placement' @::@ 'Placement'
--
-- * 'i1Platform' @::@ 'Maybe' 'PlatformValues'
--
-- * 'i1PrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'i1PrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'i1ProductCodes' @::@ ['ProductCode']
--
-- * 'i1PublicDnsName' @::@ 'Maybe' 'Text'
--
-- * 'i1PublicIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'i1RamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'i1RootDeviceName' @::@ 'Maybe' 'Text'
--
-- * 'i1RootDeviceType' @::@ 'DeviceType'
--
-- * 'i1SecurityGroups' @::@ ['GroupIdentifier']
--
-- * 'i1SourceDestCheck' @::@ 'Maybe' 'Bool'
--
-- * 'i1SpotInstanceRequestId' @::@ 'Maybe' 'Text'
--
-- * 'i1SriovNetSupport' @::@ 'Maybe' 'Text'
--
-- * 'i1State' @::@ 'InstanceState'
--
-- * 'i1StateReason' @::@ 'Maybe' 'StateReason'
--
-- * 'i1StateTransitionReason' @::@ 'Maybe' 'Text'
--
-- * 'i1SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'i1Tags' @::@ ['Tag']
--
-- * 'i1VirtualizationType' @::@ 'VirtualizationType'
--
-- * 'i1VpcId' @::@ 'Maybe' 'Text'
--
instance' :: Text -- ^ 'i1InstanceId'
          -> Text -- ^ 'i1ImageId'
          -> InstanceState -- ^ 'i1State'
          -> Int -- ^ 'i1AmiLaunchIndex'
          -> InstanceType -- ^ 'i1InstanceType'
          -> UTCTime -- ^ 'i1LaunchTime'
          -> Placement -- ^ 'i1Placement'
          -> Monitoring -- ^ 'i1Monitoring'
          -> ArchitectureValues -- ^ 'i1Architecture'
          -> DeviceType -- ^ 'i1RootDeviceType'
          -> VirtualizationType -- ^ 'i1VirtualizationType'
          -> HypervisorType -- ^ 'i1Hypervisor'
          -> Bool -- ^ 'i1EbsOptimized'
          -> Instance
instance' p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 = Instance
    { _i1InstanceId            = p1
    , _i1ImageId               = p2
    , _i1State                 = p3
    , _i1AmiLaunchIndex        = p4
    , _i1InstanceType          = p5
    , _i1LaunchTime            = withIso _Time (const id) p6
    , _i1Placement             = p7
    , _i1Monitoring            = p8
    , _i1Architecture          = p9
    , _i1RootDeviceType        = p10
    , _i1VirtualizationType    = p11
    , _i1Hypervisor            = p12
    , _i1EbsOptimized          = p13
    , _i1PrivateDnsName        = Nothing
    , _i1PublicDnsName         = Nothing
    , _i1StateTransitionReason = Nothing
    , _i1KeyName               = Nothing
    , _i1ProductCodes          = mempty
    , _i1KernelId              = Nothing
    , _i1RamdiskId             = Nothing
    , _i1Platform              = Nothing
    , _i1SubnetId              = Nothing
    , _i1VpcId                 = Nothing
    , _i1PrivateIpAddress      = Nothing
    , _i1PublicIpAddress       = Nothing
    , _i1StateReason           = Nothing
    , _i1RootDeviceName        = Nothing
    , _i1BlockDeviceMappings   = mempty
    , _i1InstanceLifecycle     = Nothing
    , _i1SpotInstanceRequestId = Nothing
    , _i1ClientToken           = Nothing
    , _i1Tags                  = mempty
    , _i1SecurityGroups        = mempty
    , _i1SourceDestCheck       = Nothing
    , _i1NetworkInterfaces     = mempty
    , _i1IamInstanceProfile    = Nothing
    , _i1SriovNetSupport       = Nothing
    }

-- | The AMI launch index, which can be used to find this instance in the launch
-- group.
i1AmiLaunchIndex :: Lens' Instance Int
i1AmiLaunchIndex = lens _i1AmiLaunchIndex (\s a -> s { _i1AmiLaunchIndex = a })

-- | The architecture of the image.
i1Architecture :: Lens' Instance ArchitectureValues
i1Architecture = lens _i1Architecture (\s a -> s { _i1Architecture = a })

-- | Any block device mapping entries for the instance.
i1BlockDeviceMappings :: Lens' Instance [InstanceBlockDeviceMapping]
i1BlockDeviceMappings =
    lens _i1BlockDeviceMappings (\s a -> s { _i1BlockDeviceMappings = a })
        . _List

-- | The idempotency token you provided when you launched the instance.
i1ClientToken :: Lens' Instance (Maybe Text)
i1ClientToken = lens _i1ClientToken (\s a -> s { _i1ClientToken = a })

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal I/O performance. This optimization isn't available
-- with all instance types. Additional usage charges apply when using an EBS
-- Optimized instance.
i1EbsOptimized :: Lens' Instance Bool
i1EbsOptimized = lens _i1EbsOptimized (\s a -> s { _i1EbsOptimized = a })

-- | The hypervisor type of the instance.
i1Hypervisor :: Lens' Instance HypervisorType
i1Hypervisor = lens _i1Hypervisor (\s a -> s { _i1Hypervisor = a })

-- | The IAM instance profile associated with the instance.
i1IamInstanceProfile :: Lens' Instance (Maybe IamInstanceProfile)
i1IamInstanceProfile =
    lens _i1IamInstanceProfile (\s a -> s { _i1IamInstanceProfile = a })

-- | The ID of the AMI used to launch the instance.
i1ImageId :: Lens' Instance Text
i1ImageId = lens _i1ImageId (\s a -> s { _i1ImageId = a })

-- | The ID of the instance.
i1InstanceId :: Lens' Instance Text
i1InstanceId = lens _i1InstanceId (\s a -> s { _i1InstanceId = a })

-- | Indicates whether this is a Spot Instance.
i1InstanceLifecycle :: Lens' Instance (Maybe InstanceLifecycleType)
i1InstanceLifecycle =
    lens _i1InstanceLifecycle (\s a -> s { _i1InstanceLifecycle = a })

-- | The instance type.
i1InstanceType :: Lens' Instance InstanceType
i1InstanceType = lens _i1InstanceType (\s a -> s { _i1InstanceType = a })

-- | The kernel associated with this instance.
i1KernelId :: Lens' Instance (Maybe Text)
i1KernelId = lens _i1KernelId (\s a -> s { _i1KernelId = a })

-- | The name of the key pair, if this instance was launched with an associated
-- key pair.
i1KeyName :: Lens' Instance (Maybe Text)
i1KeyName = lens _i1KeyName (\s a -> s { _i1KeyName = a })

-- | The time the instance was launched.
i1LaunchTime :: Lens' Instance UTCTime
i1LaunchTime = lens _i1LaunchTime (\s a -> s { _i1LaunchTime = a }) . _Time

-- | The monitoring information for the instance.
i1Monitoring :: Lens' Instance Monitoring
i1Monitoring = lens _i1Monitoring (\s a -> s { _i1Monitoring = a })

-- | [EC2-VPC] One or more network interfaces for the instance.
i1NetworkInterfaces :: Lens' Instance [InstanceNetworkInterface]
i1NetworkInterfaces =
    lens _i1NetworkInterfaces (\s a -> s { _i1NetworkInterfaces = a })
        . _List

-- | The location where the instance launched.
i1Placement :: Lens' Instance Placement
i1Placement = lens _i1Placement (\s a -> s { _i1Placement = a })

-- | The value is 'Windows' for Windows instances; otherwise blank.
i1Platform :: Lens' Instance (Maybe PlatformValues)
i1Platform = lens _i1Platform (\s a -> s { _i1Platform = a })

-- | The private DNS name assigned to the instance. This DNS name can only be used
-- inside the Amazon EC2 network. This name is not available until the instance
-- enters the 'running' state.
i1PrivateDnsName :: Lens' Instance (Maybe Text)
i1PrivateDnsName = lens _i1PrivateDnsName (\s a -> s { _i1PrivateDnsName = a })

-- | The private IP address assigned to the instance.
i1PrivateIpAddress :: Lens' Instance (Maybe Text)
i1PrivateIpAddress =
    lens _i1PrivateIpAddress (\s a -> s { _i1PrivateIpAddress = a })

-- | The product codes attached to this instance.
i1ProductCodes :: Lens' Instance [ProductCode]
i1ProductCodes = lens _i1ProductCodes (\s a -> s { _i1ProductCodes = a }) . _List

-- | The public DNS name assigned to the instance. This name is not available
-- until the instance enters the 'running' state.
i1PublicDnsName :: Lens' Instance (Maybe Text)
i1PublicDnsName = lens _i1PublicDnsName (\s a -> s { _i1PublicDnsName = a })

-- | The public IP address assigned to the instance.
i1PublicIpAddress :: Lens' Instance (Maybe Text)
i1PublicIpAddress =
    lens _i1PublicIpAddress (\s a -> s { _i1PublicIpAddress = a })

-- | The RAM disk associated with this instance.
i1RamdiskId :: Lens' Instance (Maybe Text)
i1RamdiskId = lens _i1RamdiskId (\s a -> s { _i1RamdiskId = a })

-- | The root device name (for example, '/dev/sda1' or '/dev/xvda').
i1RootDeviceName :: Lens' Instance (Maybe Text)
i1RootDeviceName = lens _i1RootDeviceName (\s a -> s { _i1RootDeviceName = a })

-- | The root device type used by the AMI. The AMI can use an EBS volume or an
-- instance store volume.
i1RootDeviceType :: Lens' Instance DeviceType
i1RootDeviceType = lens _i1RootDeviceType (\s a -> s { _i1RootDeviceType = a })

-- | One or more security groups for the instance.
i1SecurityGroups :: Lens' Instance [GroupIdentifier]
i1SecurityGroups = lens _i1SecurityGroups (\s a -> s { _i1SecurityGroups = a }) . _List

-- | Specifies whether to enable an instance launched in a VPC to perform NAT.
-- This controls whether source/destination checking is enabled on the instance.
-- A value of 'true' means checking is enabled, and 'false' means checking is
-- disabled. The value must be 'false' for the instance to perform NAT. For more
-- information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/.
i1SourceDestCheck :: Lens' Instance (Maybe Bool)
i1SourceDestCheck =
    lens _i1SourceDestCheck (\s a -> s { _i1SourceDestCheck = a })

-- | The ID of the Spot Instance request.
i1SpotInstanceRequestId :: Lens' Instance (Maybe Text)
i1SpotInstanceRequestId =
    lens _i1SpotInstanceRequestId (\s a -> s { _i1SpotInstanceRequestId = a })

-- | Specifies whether enhanced networking is enabled.
i1SriovNetSupport :: Lens' Instance (Maybe Text)
i1SriovNetSupport =
    lens _i1SriovNetSupport (\s a -> s { _i1SriovNetSupport = a })

-- | The current state of the instance.
i1State :: Lens' Instance InstanceState
i1State = lens _i1State (\s a -> s { _i1State = a })

-- | The reason for the most recent state transition.
i1StateReason :: Lens' Instance (Maybe StateReason)
i1StateReason = lens _i1StateReason (\s a -> s { _i1StateReason = a })

-- | The reason for the most recent state transition. This might be an empty
-- string.
i1StateTransitionReason :: Lens' Instance (Maybe Text)
i1StateTransitionReason =
    lens _i1StateTransitionReason (\s a -> s { _i1StateTransitionReason = a })

-- | The ID of the subnet in which the instance is running.
i1SubnetId :: Lens' Instance (Maybe Text)
i1SubnetId = lens _i1SubnetId (\s a -> s { _i1SubnetId = a })

-- | Any tags assigned to the instance.
i1Tags :: Lens' Instance [Tag]
i1Tags = lens _i1Tags (\s a -> s { _i1Tags = a }) . _List

-- | The virtualization type of the instance.
i1VirtualizationType :: Lens' Instance VirtualizationType
i1VirtualizationType =
    lens _i1VirtualizationType (\s a -> s { _i1VirtualizationType = a })

-- | The ID of the VPC in which the instance is running.
i1VpcId :: Lens' Instance (Maybe Text)
i1VpcId = lens _i1VpcId (\s a -> s { _i1VpcId = a })

instance FromXML Instance where
    parseXML x = Instance
        <$> x .@  "amiLaunchIndex"
        <*> x .@  "architecture"
        <*> x .@? "blockDeviceMapping" .!@ mempty
        <*> x .@? "clientToken"
        <*> x .@  "ebsOptimized"
        <*> x .@  "hypervisor"
        <*> x .@? "iamInstanceProfile"
        <*> x .@  "imageId"
        <*> x .@  "instanceId"
        <*> x .@? "instanceLifecycle"
        <*> x .@  "instanceType"
        <*> x .@? "kernelId"
        <*> x .@? "keyName"
        <*> x .@  "launchTime"
        <*> x .@  "monitoring"
        <*> x .@? "networkInterfaceSet" .!@ mempty
        <*> x .@  "placement"
        <*> x .@? "platform"
        <*> x .@? "privateDnsName"
        <*> x .@? "privateIpAddress"
        <*> x .@? "productCodes" .!@ mempty
        <*> x .@? "dnsName"
        <*> x .@? "ipAddress"
        <*> x .@? "ramdiskId"
        <*> x .@? "rootDeviceName"
        <*> x .@  "rootDeviceType"
        <*> x .@? "groupSet" .!@ mempty
        <*> x .@? "sourceDestCheck"
        <*> x .@? "spotInstanceRequestId"
        <*> x .@? "sriovNetSupport"
        <*> x .@  "instanceState"
        <*> x .@? "stateReason"
        <*> x .@? "reason"
        <*> x .@? "subnetId"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "virtualizationType"
        <*> x .@? "vpcId"

instance ToQuery Instance where
    toQuery Instance{..} = mconcat
        [ "AmiLaunchIndex"        =? _i1AmiLaunchIndex
        , "Architecture"          =? _i1Architecture
        , "BlockDeviceMapping"    `toQueryList` _i1BlockDeviceMappings
        , "ClientToken"           =? _i1ClientToken
        , "EbsOptimized"          =? _i1EbsOptimized
        , "Hypervisor"            =? _i1Hypervisor
        , "IamInstanceProfile"    =? _i1IamInstanceProfile
        , "ImageId"               =? _i1ImageId
        , "InstanceId"            =? _i1InstanceId
        , "InstanceLifecycle"     =? _i1InstanceLifecycle
        , "InstanceType"          =? _i1InstanceType
        , "KernelId"              =? _i1KernelId
        , "KeyName"               =? _i1KeyName
        , "LaunchTime"            =? _i1LaunchTime
        , "Monitoring"            =? _i1Monitoring
        , "NetworkInterfaceSet"   `toQueryList` _i1NetworkInterfaces
        , "Placement"             =? _i1Placement
        , "Platform"              =? _i1Platform
        , "PrivateDnsName"        =? _i1PrivateDnsName
        , "PrivateIpAddress"      =? _i1PrivateIpAddress
        , "ProductCodes"          `toQueryList` _i1ProductCodes
        , "DnsName"               =? _i1PublicDnsName
        , "IpAddress"             =? _i1PublicIpAddress
        , "RamdiskId"             =? _i1RamdiskId
        , "RootDeviceName"        =? _i1RootDeviceName
        , "RootDeviceType"        =? _i1RootDeviceType
        , "GroupSet"              `toQueryList` _i1SecurityGroups
        , "SourceDestCheck"       =? _i1SourceDestCheck
        , "SpotInstanceRequestId" =? _i1SpotInstanceRequestId
        , "SriovNetSupport"       =? _i1SriovNetSupport
        , "InstanceState"         =? _i1State
        , "StateReason"           =? _i1StateReason
        , "Reason"                =? _i1StateTransitionReason
        , "SubnetId"              =? _i1SubnetId
        , "TagSet"                `toQueryList` _i1Tags
        , "VirtualizationType"    =? _i1VirtualizationType
        , "VpcId"                 =? _i1VpcId
        ]

data ExportTask = ExportTask
    { _etDescription           :: Text
    , _etExportTaskId          :: Text
    , _etExportToS3Task        :: ExportToS3Task
    , _etInstanceExportDetails :: InstanceExportDetails
    , _etState                 :: ExportTaskState
    , _etStatusMessage         :: Text
    } deriving (Eq, Read, Show)

-- | 'ExportTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etDescription' @::@ 'Text'
--
-- * 'etExportTaskId' @::@ 'Text'
--
-- * 'etExportToS3Task' @::@ 'ExportToS3Task'
--
-- * 'etInstanceExportDetails' @::@ 'InstanceExportDetails'
--
-- * 'etState' @::@ 'ExportTaskState'
--
-- * 'etStatusMessage' @::@ 'Text'
--
exportTask :: Text -- ^ 'etExportTaskId'
           -> Text -- ^ 'etDescription'
           -> ExportTaskState -- ^ 'etState'
           -> Text -- ^ 'etStatusMessage'
           -> InstanceExportDetails -- ^ 'etInstanceExportDetails'
           -> ExportToS3Task -- ^ 'etExportToS3Task'
           -> ExportTask
exportTask p1 p2 p3 p4 p5 p6 = ExportTask
    { _etExportTaskId          = p1
    , _etDescription           = p2
    , _etState                 = p3
    , _etStatusMessage         = p4
    , _etInstanceExportDetails = p5
    , _etExportToS3Task        = p6
    }

-- | A description of the resource being exported.
etDescription :: Lens' ExportTask Text
etDescription = lens _etDescription (\s a -> s { _etDescription = a })

-- | The ID of the export task.
etExportTaskId :: Lens' ExportTask Text
etExportTaskId = lens _etExportTaskId (\s a -> s { _etExportTaskId = a })

-- | Information about the export task.
etExportToS3Task :: Lens' ExportTask ExportToS3Task
etExportToS3Task = lens _etExportToS3Task (\s a -> s { _etExportToS3Task = a })

-- | Information about the instance to export.
etInstanceExportDetails :: Lens' ExportTask InstanceExportDetails
etInstanceExportDetails =
    lens _etInstanceExportDetails (\s a -> s { _etInstanceExportDetails = a })

-- | The state of the export task.
etState :: Lens' ExportTask ExportTaskState
etState = lens _etState (\s a -> s { _etState = a })

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask Text
etStatusMessage = lens _etStatusMessage (\s a -> s { _etStatusMessage = a })

instance FromXML ExportTask where
    parseXML x = ExportTask
        <$> x .@  "description"
        <*> x .@  "exportTaskId"
        <*> x .@  "exportToS3"
        <*> x .@  "instanceExport"
        <*> x .@  "state"
        <*> x .@  "statusMessage"

instance ToQuery ExportTask where
    toQuery ExportTask{..} = mconcat
        [ "Description"    =? _etDescription
        , "ExportTaskId"   =? _etExportTaskId
        , "ExportToS3"     =? _etExportToS3Task
        , "InstanceExport" =? _etInstanceExportDetails
        , "State"          =? _etState
        , "StatusMessage"  =? _etStatusMessage
        ]

data ResetImageAttributeName
    = RIANLaunchPermission -- ^ launchPermission
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ResetImageAttributeName

instance FromText ResetImageAttributeName where
    parser = takeLowerText >>= \case
        "launchpermission" -> pure RIANLaunchPermission
        e                  -> fail $
            "Failure parsing ResetImageAttributeName from " ++ show e

instance ToText ResetImageAttributeName where
    toText RIANLaunchPermission = "launchPermission"

instance ToByteString ResetImageAttributeName
instance ToHeader     ResetImageAttributeName
instance ToQuery      ResetImageAttributeName

instance FromXML ResetImageAttributeName where
    parseXML = parseXMLText "ResetImageAttributeName"

data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification
    { _rslsAddressingType      :: Maybe Text
    , _rslsBlockDeviceMappings :: List "item" BlockDeviceMapping
    , _rslsEbsOptimized        :: Maybe Bool
    , _rslsIamInstanceProfile  :: Maybe IamInstanceProfileSpecification
    , _rslsImageId             :: Maybe Text
    , _rslsInstanceType        :: Maybe InstanceType
    , _rslsKernelId            :: Maybe Text
    , _rslsKeyName             :: Maybe Text
    , _rslsMonitoring          :: Maybe RunInstancesMonitoringEnabled
    , _rslsNetworkInterfaces   :: List "item" InstanceNetworkInterfaceSpecification
    , _rslsPlacement           :: Maybe SpotPlacement
    , _rslsRamdiskId           :: Maybe Text
    , _rslsSecurityGroupIds    :: List "item" Text
    , _rslsSecurityGroups      :: List "item" Text
    , _rslsSubnetId            :: Maybe Text
    , _rslsUserData            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'RequestSpotLaunchSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rslsAddressingType' @::@ 'Maybe' 'Text'
--
-- * 'rslsBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'rslsEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'rslsIamInstanceProfile' @::@ 'Maybe' 'IamInstanceProfileSpecification'
--
-- * 'rslsImageId' @::@ 'Maybe' 'Text'
--
-- * 'rslsInstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'rslsKernelId' @::@ 'Maybe' 'Text'
--
-- * 'rslsKeyName' @::@ 'Maybe' 'Text'
--
-- * 'rslsMonitoring' @::@ 'Maybe' 'RunInstancesMonitoringEnabled'
--
-- * 'rslsNetworkInterfaces' @::@ ['InstanceNetworkInterfaceSpecification']
--
-- * 'rslsPlacement' @::@ 'Maybe' 'SpotPlacement'
--
-- * 'rslsRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'rslsSecurityGroupIds' @::@ ['Text']
--
-- * 'rslsSecurityGroups' @::@ ['Text']
--
-- * 'rslsSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'rslsUserData' @::@ 'Maybe' 'Text'
--
requestSpotLaunchSpecification :: RequestSpotLaunchSpecification
requestSpotLaunchSpecification = RequestSpotLaunchSpecification
    { _rslsImageId             = Nothing
    , _rslsKeyName             = Nothing
    , _rslsSecurityGroups      = mempty
    , _rslsUserData            = Nothing
    , _rslsAddressingType      = Nothing
    , _rslsInstanceType        = Nothing
    , _rslsPlacement           = Nothing
    , _rslsKernelId            = Nothing
    , _rslsRamdiskId           = Nothing
    , _rslsBlockDeviceMappings = mempty
    , _rslsSubnetId            = Nothing
    , _rslsNetworkInterfaces   = mempty
    , _rslsIamInstanceProfile  = Nothing
    , _rslsEbsOptimized        = Nothing
    , _rslsMonitoring          = Nothing
    , _rslsSecurityGroupIds    = mempty
    }

-- | Deprecated.
rslsAddressingType :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsAddressingType =
    lens _rslsAddressingType (\s a -> s { _rslsAddressingType = a })

-- | One or more block device mapping entries.
rslsBlockDeviceMappings :: Lens' RequestSpotLaunchSpecification [BlockDeviceMapping]
rslsBlockDeviceMappings =
    lens _rslsBlockDeviceMappings (\s a -> s { _rslsBlockDeviceMappings = a })
        . _List

-- | Indicates whether the instance is optimized for EBS I/O. This optimization
-- provides dedicated throughput to Amazon EBS and an optimized configuration
-- stack to provide optimal EBS I/O performance. This optimization isn't
-- available with all instance types. Additional usage charges apply when using
-- an EBS Optimized instance.
--
-- Default: 'false'
rslsEbsOptimized :: Lens' RequestSpotLaunchSpecification (Maybe Bool)
rslsEbsOptimized = lens _rslsEbsOptimized (\s a -> s { _rslsEbsOptimized = a })

-- | The IAM instance profile.
rslsIamInstanceProfile :: Lens' RequestSpotLaunchSpecification (Maybe IamInstanceProfileSpecification)
rslsIamInstanceProfile =
    lens _rslsIamInstanceProfile (\s a -> s { _rslsIamInstanceProfile = a })

-- | The ID of the AMI.
rslsImageId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsImageId = lens _rslsImageId (\s a -> s { _rslsImageId = a })

-- | The instance type.
rslsInstanceType :: Lens' RequestSpotLaunchSpecification (Maybe InstanceType)
rslsInstanceType = lens _rslsInstanceType (\s a -> s { _rslsInstanceType = a })

-- | The ID of the kernel.
rslsKernelId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKernelId = lens _rslsKernelId (\s a -> s { _rslsKernelId = a })

-- | The name of the key pair.
rslsKeyName :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKeyName = lens _rslsKeyName (\s a -> s { _rslsKeyName = a })

rslsMonitoring :: Lens' RequestSpotLaunchSpecification (Maybe RunInstancesMonitoringEnabled)
rslsMonitoring = lens _rslsMonitoring (\s a -> s { _rslsMonitoring = a })

-- | One or more network interfaces.
rslsNetworkInterfaces :: Lens' RequestSpotLaunchSpecification [InstanceNetworkInterfaceSpecification]
rslsNetworkInterfaces =
    lens _rslsNetworkInterfaces (\s a -> s { _rslsNetworkInterfaces = a })
        . _List

-- | The placement information for the instance.
rslsPlacement :: Lens' RequestSpotLaunchSpecification (Maybe SpotPlacement)
rslsPlacement = lens _rslsPlacement (\s a -> s { _rslsPlacement = a })

-- | The ID of the RAM disk.
rslsRamdiskId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsRamdiskId = lens _rslsRamdiskId (\s a -> s { _rslsRamdiskId = a })

rslsSecurityGroupIds :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroupIds =
    lens _rslsSecurityGroupIds (\s a -> s { _rslsSecurityGroupIds = a })
        . _List

rslsSecurityGroups :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroups =
    lens _rslsSecurityGroups (\s a -> s { _rslsSecurityGroups = a })
        . _List

-- | The ID of the subnet in which to launch the instance.
rslsSubnetId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsSubnetId = lens _rslsSubnetId (\s a -> s { _rslsSubnetId = a })

-- | The Base64-encoded MIME user data to make available to the instances.
rslsUserData :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsUserData = lens _rslsUserData (\s a -> s { _rslsUserData = a })

instance FromXML RequestSpotLaunchSpecification where
    parseXML x = RequestSpotLaunchSpecification
        <$> x .@? "addressingType"
        <*> x .@? "blockDeviceMapping" .!@ mempty
        <*> x .@? "ebsOptimized"
        <*> x .@? "iamInstanceProfile"
        <*> x .@? "imageId"
        <*> x .@? "instanceType"
        <*> x .@? "kernelId"
        <*> x .@? "keyName"
        <*> x .@? "monitoring"
        <*> x .@? "NetworkInterface" .!@ mempty
        <*> x .@? "placement"
        <*> x .@? "ramdiskId"
        <*> x .@? "SecurityGroupId" .!@ mempty
        <*> x .@? "SecurityGroup" .!@ mempty
        <*> x .@? "subnetId"
        <*> x .@? "userData"

instance ToQuery RequestSpotLaunchSpecification where
    toQuery RequestSpotLaunchSpecification{..} = mconcat
        [ "AddressingType"     =? _rslsAddressingType
        , "BlockDeviceMapping" `toQueryList` _rslsBlockDeviceMappings
        , "EbsOptimized"       =? _rslsEbsOptimized
        , "IamInstanceProfile" =? _rslsIamInstanceProfile
        , "ImageId"            =? _rslsImageId
        , "InstanceType"       =? _rslsInstanceType
        , "KernelId"           =? _rslsKernelId
        , "KeyName"            =? _rslsKeyName
        , "Monitoring"         =? _rslsMonitoring
        , "NetworkInterface"   `toQueryList` _rslsNetworkInterfaces
        , "Placement"          =? _rslsPlacement
        , "RamdiskId"          =? _rslsRamdiskId
        , "SecurityGroupId"    `toQueryList` _rslsSecurityGroupIds
        , "SecurityGroup"      `toQueryList` _rslsSecurityGroups
        , "SubnetId"           =? _rslsSubnetId
        , "UserData"           =? _rslsUserData
        ]

newtype VolumeDetail = VolumeDetail
    { _vdSize :: Integer
    } deriving (Eq, Ord, Read, Show, Enum, Num, Integral, Real)

-- | 'VolumeDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdSize' @::@ 'Integer'
--
volumeDetail :: Integer -- ^ 'vdSize'
             -> VolumeDetail
volumeDetail p1 = VolumeDetail
    { _vdSize = p1
    }

-- | The size of the volume, in GiB.
vdSize :: Lens' VolumeDetail Integer
vdSize = lens _vdSize (\s a -> s { _vdSize = a })

instance FromXML VolumeDetail where
    parseXML x = VolumeDetail
        <$> x .@  "size"

instance ToQuery VolumeDetail where
    toQuery VolumeDetail{..} = mconcat
        [ "Size" =? _vdSize
        ]

data PricingDetail = PricingDetail
    { _pdCount :: Maybe Int
    , _pdPrice :: Maybe Double
    } deriving (Eq, Ord, Read, Show)

-- | 'PricingDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdCount' @::@ 'Maybe' 'Int'
--
-- * 'pdPrice' @::@ 'Maybe' 'Double'
--
pricingDetail :: PricingDetail
pricingDetail = PricingDetail
    { _pdPrice = Nothing
    , _pdCount = Nothing
    }

-- | The number of instances available for the price.
pdCount :: Lens' PricingDetail (Maybe Int)
pdCount = lens _pdCount (\s a -> s { _pdCount = a })

-- | The price per instance.
pdPrice :: Lens' PricingDetail (Maybe Double)
pdPrice = lens _pdPrice (\s a -> s { _pdPrice = a })

instance FromXML PricingDetail where
    parseXML x = PricingDetail
        <$> x .@? "count"
        <*> x .@? "price"

instance ToQuery PricingDetail where
    toQuery PricingDetail{..} = mconcat
        [ "Count" =? _pdCount
        , "Price" =? _pdPrice
        ]

data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaAssociation      :: Maybe NetworkInterfaceAssociation
    , _nipiaPrimary          :: Maybe Bool
    , _nipiaPrivateDnsName   :: Maybe Text
    , _nipiaPrivateIpAddress :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'NetworkInterfacePrivateIpAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nipiaAssociation' @::@ 'Maybe' 'NetworkInterfaceAssociation'
--
-- * 'nipiaPrimary' @::@ 'Maybe' 'Bool'
--
-- * 'nipiaPrivateDnsName' @::@ 'Maybe' 'Text'
--
-- * 'nipiaPrivateIpAddress' @::@ 'Maybe' 'Text'
--
networkInterfacePrivateIpAddress :: NetworkInterfacePrivateIpAddress
networkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { _nipiaPrivateIpAddress = Nothing
    , _nipiaPrivateDnsName   = Nothing
    , _nipiaPrimary          = Nothing
    , _nipiaAssociation      = Nothing
    }

-- | The association information for an Elastic IP address associated with the
-- network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIpAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation = lens _nipiaAssociation (\s a -> s { _nipiaAssociation = a })

-- | Indicates whether this IP address is the primary private IP address of the
-- network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIpAddress (Maybe Bool)
nipiaPrimary = lens _nipiaPrimary (\s a -> s { _nipiaPrimary = a })

-- | The private DNS name.
nipiaPrivateDnsName :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateDnsName =
    lens _nipiaPrivateDnsName (\s a -> s { _nipiaPrivateDnsName = a })

-- | The private IP address.
nipiaPrivateIpAddress :: Lens' NetworkInterfacePrivateIpAddress (Maybe Text)
nipiaPrivateIpAddress =
    lens _nipiaPrivateIpAddress (\s a -> s { _nipiaPrivateIpAddress = a })

instance FromXML NetworkInterfacePrivateIpAddress where
    parseXML x = NetworkInterfacePrivateIpAddress
        <$> x .@? "association"
        <*> x .@? "primary"
        <*> x .@? "privateDnsName"
        <*> x .@? "privateIpAddress"

instance ToQuery NetworkInterfacePrivateIpAddress where
    toQuery NetworkInterfacePrivateIpAddress{..} = mconcat
        [ "Association"      =? _nipiaAssociation
        , "Primary"          =? _nipiaPrimary
        , "PrivateDnsName"   =? _nipiaPrivateDnsName
        , "PrivateIpAddress" =? _nipiaPrivateIpAddress
        ]

data DiskImageFormat
    = Raw  -- ^ RAW
    | Vhd  -- ^ VHD
    | Vmdk -- ^ VMDK
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DiskImageFormat

instance FromText DiskImageFormat where
    parser = takeLowerText >>= \case
        "raw"  -> pure Raw
        "vhd"  -> pure Vhd
        "vmdk" -> pure Vmdk
        e      -> fail $
            "Failure parsing DiskImageFormat from " ++ show e

instance ToText DiskImageFormat where
    toText = \case
        Raw  -> "RAW"
        Vhd  -> "VHD"
        Vmdk -> "VMDK"

instance ToByteString DiskImageFormat
instance ToHeader     DiskImageFormat
instance ToQuery      DiskImageFormat

instance FromXML DiskImageFormat where
    parseXML = parseXMLText "DiskImageFormat"

data BundleTaskError = BundleTaskError
    { _bteCode    :: Maybe Text
    , _bteMessage :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'BundleTaskError' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bteCode' @::@ 'Maybe' 'Text'
--
-- * 'bteMessage' @::@ 'Maybe' 'Text'
--
bundleTaskError :: BundleTaskError
bundleTaskError = BundleTaskError
    { _bteCode    = Nothing
    , _bteMessage = Nothing
    }

-- | The error code.
bteCode :: Lens' BundleTaskError (Maybe Text)
bteCode = lens _bteCode (\s a -> s { _bteCode = a })

-- | The error message.
bteMessage :: Lens' BundleTaskError (Maybe Text)
bteMessage = lens _bteMessage (\s a -> s { _bteMessage = a })

instance FromXML BundleTaskError where
    parseXML x = BundleTaskError
        <$> x .@? "code"
        <*> x .@? "message"

instance ToQuery BundleTaskError where
    toQuery BundleTaskError{..} = mconcat
        [ "Code"    =? _bteCode
        , "Message" =? _bteMessage
        ]

data VpcClassicLink = VpcClassicLink
    { _vclClassicLinkEnabled :: Maybe Bool
    , _vclTags               :: List "item" Tag
    , _vclVpcId              :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'VpcClassicLink' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vclClassicLinkEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'vclTags' @::@ ['Tag']
--
-- * 'vclVpcId' @::@ 'Maybe' 'Text'
--
vpcClassicLink :: VpcClassicLink
vpcClassicLink = VpcClassicLink
    { _vclVpcId              = Nothing
    , _vclClassicLinkEnabled = Nothing
    , _vclTags               = mempty
    }

-- | Indicates whether the VPC is enabled for ClassicLink.
vclClassicLinkEnabled :: Lens' VpcClassicLink (Maybe Bool)
vclClassicLinkEnabled =
    lens _vclClassicLinkEnabled (\s a -> s { _vclClassicLinkEnabled = a })

-- | Any tags assigned to the VPC.
vclTags :: Lens' VpcClassicLink [Tag]
vclTags = lens _vclTags (\s a -> s { _vclTags = a }) . _List

-- | The ID of the VPC.
vclVpcId :: Lens' VpcClassicLink (Maybe Text)
vclVpcId = lens _vclVpcId (\s a -> s { _vclVpcId = a })

instance FromXML VpcClassicLink where
    parseXML x = VpcClassicLink
        <$> x .@? "classicLinkEnabled"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@? "vpcId"

instance ToQuery VpcClassicLink where
    toQuery VpcClassicLink{..} = mconcat
        [ "ClassicLinkEnabled" =? _vclClassicLinkEnabled
        , "TagSet"             `toQueryList` _vclTags
        , "VpcId"              =? _vclVpcId
        ]

data VolumeStatusItem = VolumeStatusItem
    { _vsiActions          :: List "item" VolumeStatusAction
    , _vsiAvailabilityZone :: Maybe Text
    , _vsiEvents           :: List "item" VolumeStatusEvent
    , _vsiVolumeId         :: Maybe Text
    , _vsiVolumeStatus     :: Maybe VolumeStatusInfo
    } deriving (Eq, Read, Show)

-- | 'VolumeStatusItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsiActions' @::@ ['VolumeStatusAction']
--
-- * 'vsiAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'vsiEvents' @::@ ['VolumeStatusEvent']
--
-- * 'vsiVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'vsiVolumeStatus' @::@ 'Maybe' 'VolumeStatusInfo'
--
volumeStatusItem :: VolumeStatusItem
volumeStatusItem = VolumeStatusItem
    { _vsiVolumeId         = Nothing
    , _vsiAvailabilityZone = Nothing
    , _vsiVolumeStatus     = Nothing
    , _vsiEvents           = mempty
    , _vsiActions          = mempty
    }

-- | The details of the operation.
vsiActions :: Lens' VolumeStatusItem [VolumeStatusAction]
vsiActions = lens _vsiActions (\s a -> s { _vsiActions = a }) . _List

-- | The Availability Zone of the volume.
vsiAvailabilityZone :: Lens' VolumeStatusItem (Maybe Text)
vsiAvailabilityZone =
    lens _vsiAvailabilityZone (\s a -> s { _vsiAvailabilityZone = a })

-- | A list of events associated with the volume.
vsiEvents :: Lens' VolumeStatusItem [VolumeStatusEvent]
vsiEvents = lens _vsiEvents (\s a -> s { _vsiEvents = a }) . _List

-- | The volume ID.
vsiVolumeId :: Lens' VolumeStatusItem (Maybe Text)
vsiVolumeId = lens _vsiVolumeId (\s a -> s { _vsiVolumeId = a })

-- | The volume status.
vsiVolumeStatus :: Lens' VolumeStatusItem (Maybe VolumeStatusInfo)
vsiVolumeStatus = lens _vsiVolumeStatus (\s a -> s { _vsiVolumeStatus = a })

instance FromXML VolumeStatusItem where
    parseXML x = VolumeStatusItem
        <$> x .@? "actionsSet" .!@ mempty
        <*> x .@? "availabilityZone"
        <*> x .@? "eventsSet" .!@ mempty
        <*> x .@? "volumeId"
        <*> x .@? "volumeStatus"

instance ToQuery VolumeStatusItem where
    toQuery VolumeStatusItem{..} = mconcat
        [ "ActionsSet"       `toQueryList` _vsiActions
        , "AvailabilityZone" =? _vsiAvailabilityZone
        , "EventsSet"        `toQueryList` _vsiEvents
        , "VolumeId"         =? _vsiVolumeId
        , "VolumeStatus"     =? _vsiVolumeStatus
        ]
