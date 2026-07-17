export interface Job {
  id: string;
  company: string;
  company_website: string;
  origin: string;
  title: string;
  location: string;
  description_text: string;
  description_snippet: string;
  url: string;
  source: string;
  posted_at: string | null; // ISO 8601, or null for unconfirmed (DOM-fallback) jobs
}

export interface Meta {
  generated_at: string;
  window_hours: number;
  companies_checked: number;
  companies_with_matches: string[];
  companies_with_unconfirmed: string[];
  companies_dom_fallback: string[];
  companies_failed: string[];
  total_jobs_found: number;
  total_unconfirmed_found: number;
}
