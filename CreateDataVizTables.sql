USE [DataVizTesting]
GO
/****** Object:  Table [dbo].[AppealProcess]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[AppealProcess](
	[AppealProcessID] [smallint] NOT NULL,
	[ChildCourtID] [smallint] NOT NULL,
	[ParentCourtID] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[AppellateCriminalStructure]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[AppellateCriminalStructure](
	[AppellateCriminalStructureID] [smallint] NOT NULL,
	[AppellateCriminalStructureDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CaseloadSize]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CaseloadSize](
	[CaseloadSizeID] [smallint] NOT NULL,
	[CaseloadSizeDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CaseManagement]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CaseManagement](
	[CaseManagementID] [smallint] NOT NULL,
	[CaseManagementDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CaseType]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CaseType](
	[CaseTypeID] [smallint] NOT NULL,
	[CaseTypeDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[ChildCourt]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[ChildCourt](
	[ChildCourtID] [smallint] NOT NULL,
	[CourtName] [nvarchar](250) NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Court]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Court](
	[CourtID] [smallint] NOT NULL,
	[CourtName] [nvarchar](250) NOT NULL,
	[CourtLevelID] [nchar](10) NOT NULL,
	[CSPAggID] [nchar](10) NOT NULL,
	[FundingID] [nchar](1) NOT NULL,
	[AppealFromAdminAgency] [bit] NOT NULL,
	[Notes] [nvarchar](1000) NOT NULL,
	[Link] [nvarchar](500) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CourtCaseType]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CourtCaseType](
	[CourtCaseTypeID] [smallint] NOT NULL,
	[CourtID] [smallint] NOT NULL,
	[CaseTypeID] [smallint] NOT NULL,
	[AppealByRight] [bit] NOT NULL,
	[AppealByPermission] [bit] NOT NULL,
	[OriginalProceeding] [bit] NOT NULL,
	[InterlocutoryAppeal] [bit] NOT NULL,
	[Exclusive] [bit] NOT NULL,
	[Limited] [bit] NOT NULL,
	[MinValue] [nvarchar](50) NOT NULL,
	[MaxValue] [nvarchar](50) NOT NULL,
	[Notes] [nvarchar](1000) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CourtCourtName]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CourtCourtName](
	[CourtCourtNameID] [smallint] NOT NULL,
	[CourtNameID] [smallint] NOT NULL,
	[CourtID] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CourtLevel]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CourtLevel](
	[CourtLevelID] [nchar](10) NOT NULL,
	[CourtLevelDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CourtName]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CourtName](
	[CourtNameID] [smallint] NOT NULL,
	[CourtNameName] [nvarchar](250) NOT NULL,
	[NumberDivisions] [int] NOT NULL,
	[NumberJudges] [int] NOT NULL,
	[PanelDecisionID] [smallint] NOT NULL,
	[NumberPanels] [nvarchar](10) NOT NULL,
	[NumberOfIndividualCourts] [int] NOT NULL,
	[CaseManagementID] [smallint] NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[CSPAgg]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[CSPAgg](
	[CSPAggID] [nvarchar](4) NOT NULL,
	[CSPAggDescription] [nvarchar](50) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[DeathPenalty]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[DeathPenalty](
	[DeathPenaltyID] [smallint] NOT NULL,
	[DeathPenaltyDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Funding]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Funding](
	[FundingID] [nchar](1) NOT NULL,
	[FundingName] [nvarchar](50) NOT NULL,
	[FundingDescription] [nvarchar](50) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Neighbor]    Script Date: 9/21/2018 10:51:04 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Neighbor](
	[NeighborID] [smallint] NOT NULL,
	[USStateID] [smallint] NOT NULL,
	[NeighborUSStateID] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PanelDecision]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PanelDecision](
	[PanelDecisionID] [smallint] NOT NULL,
	[PanelDecisionDescription] [nvarchar](50) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PopulationCategory]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PopulationCategory](
	[PopulationCategoryID] [smallint] NOT NULL,
	[PopulationCategoryDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PopulationDensity]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PopulationDensity](
	[PopulationDensityID] [smallint] NOT NULL,
	[PopulationDensityDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Rural]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Rural](
	[RuralID] [smallint] NOT NULL,
	[RuralDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[sysdiagrams]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[sysdiagrams](
	[name] [sysname] NOT NULL,
	[principal_id] [int] NOT NULL,
	[diagram_id] [int] IDENTITY(1,1) NOT NULL,
	[version] [int] NULL,
	[definition] [varbinary](max) NULL,
PRIMARY KEY CLUSTERED 
(
	[diagram_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [UK_principal_name] UNIQUE NONCLUSTERED 
(
	[principal_id] ASC,
	[name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [dbo].[TrialCriminalProcessing]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[TrialCriminalProcessing](
	[TrialCriminalProcessingID] [smallint] NOT NULL,
	[TrialCriminalProcessingDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[TrialStructure]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[TrialStructure](
	[TrialStructureID] [smallint] NOT NULL,
	[TrialStructureDescription] [nvarchar](250) NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[USState]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[USState](
	[USStateID] [smallint] NOT NULL,
	[USStateName] [nvarchar](50) NOT NULL,
	[PopulationCategoryID] [smallint] NOT NULL,
	[PopulationDensityID] [smallint] NOT NULL,
	[RuralID] [smallint] NOT NULL,
	[TrialStructureID] [smallint] NOT NULL,
	[AppellateCriminalStructureID] [smallint] NOT NULL,
	[TrialCriminalProcessingID] [smallint] NOT NULL,
	[DeathPenaltyID] [smallint] NOT NULL,
	[TrialCaseloadSizeID] [smallint] NOT NULL,
	[DisplayOrder] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[USStateCourt]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[USStateCourt](
	[USStateCourtID] [smallint] NOT NULL,
	[CourtID] [smallint] NOT NULL,
	[USStateID] [smallint] NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[USStateNeighbor]    Script Date: 9/21/2018 10:51:05 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[USStateNeighbor](
	[USStateNeighborID] [smallint] NOT NULL,
	[USStateName] [nvarchar](50) NOT NULL
) ON [PRIMARY]
GO
ALTER TABLE [dbo].[Court] ADD  CONSTRAINT [DF_Court_AppealFromAdminAgency]  DEFAULT ((0)) FOR [AppealFromAdminAgency]
GO
ALTER TABLE [dbo].[Court] ADD  CONSTRAINT [DF_Court_Notes]  DEFAULT (N'') FOR [Notes]
GO
ALTER TABLE [dbo].[Court] ADD  CONSTRAINT [DF_Court_Link]  DEFAULT (N'') FOR [Link]
GO
ALTER TABLE [dbo].[CourtCaseType] ADD  CONSTRAINT [DF_CourtCaseType_Notes]  DEFAULT (N'') FOR [Notes]
GO
EXEC sys.sp_addextendedproperty @name=N'microsoft_database_tools_support', @value=1 , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'sysdiagrams'
GO
