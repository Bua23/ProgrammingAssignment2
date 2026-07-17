export interface Job {
  id: string;
  company: string;
  company_website: string;
  title: string;
  location: string;
  description_text: string;
  description_snippet: string;
  url: string;
  source: string;
  posted_at: string; // ISO 8601
}

export interface Meta {
  generated_at: string;
  window_hours: number;
  companies_checked: number;
  companies_with_matches: string[];
  companies_failed: string[];
  total_jobs_found: number;
}
